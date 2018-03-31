/*
 * Copyright 2018 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam

import scodec.bits._
import scodec._

import scala.annotation.tailrec

import scala.collection.immutable.VectorBuilder

package object binary {

  val noop: Codec[Unit] = new Codec[Unit] {
    def decode(bits: BitVector): Attempt[DecodeResult[Unit]] = Attempt.successful(DecodeResult((), bits))
    def encode(value: Unit): Attempt[BitVector] = Attempt.successful(BitVector.empty)
    def sizeBound: SizeBound = SizeBound.exact(0l)
  }

  def vectorLookahead[T](cond: Decoder[Boolean], element: Codec[T]): Codec[Vector[T]] =
    new Codec[Vector[T]] {

      def sizeBound: SizeBound = SizeBound.unknown

      def encode(seq: Vector[T]): Attempt[BitVector] = {
        val buf = new collection.mutable.ArrayBuffer[BitVector](seq.size)
        seq foreach { a =>
          element.encode(a) match {
            case Attempt.Successful(aa) => buf += aa
            case Attempt.Failure(err)   => return Attempt.failure(err.pushContext(buf.size.toString))
          }
        }
        def merge(offset: Int, size: Int): BitVector = size match {
          case 0 => BitVector.empty
          case 1 => buf(offset)
          case n =>
            val half = size / 2
            merge(offset, half) ++ merge(offset + half, half + (if (size % 2 == 0) 0 else 1))
        }
        Attempt.successful(merge(0, buf.size))
      }

      def decode(bits: BitVector): Attempt[DecodeResult[Vector[T]]] = {
        val builder = new VectorBuilder[T]
        @tailrec
        def loop(bits: BitVector): Attempt[DecodeResult[Vector[T]]] =
          cond.decodeValue(bits) match {
            case Attempt.Successful(true) =>
              element.decode(bits) match {
                case Attempt.Successful(DecodeResult(elem, remainder)) =>
                  builder += elem
                  loop(remainder)
                case Attempt.Failure(e) => Attempt.Failure(e)
              }
            case Attempt.Successful(false) =>
              Attempt.Successful(DecodeResult(builder.result, bits))
            case Attempt.Failure(e) =>
              Attempt.failure(e)
          }
        loop(bits)
      }

    }

  val varuint1: Codec[Int] = new Varuint(1)

  val varuint7: Codec[Int] = new Varuint(7)

  val varint7: Codec[Int] = new Varint(7)

  val varuint32: Codec[Int] = new Varuint(31)

  val varint32: Codec[Int] = new Varint(32)

  val varint64: Codec[Long] = new Varlong(64)

}
