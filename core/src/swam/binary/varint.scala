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

package swam.binary

import scodec._
import scodec.bits._

import scala.annotation.tailrec

private class Varuint(bits: Int) extends Codec[Int] {

  private val bitsL = bits.toLong

  val MaxValue = (1L << bits) - 1L
  val MinValue = 0L

  private def description = s"$bits-bit unsigned leb128 integer"

  def decode(buffer: BitVector): Attempt[DecodeResult[Int]] =
    if (buffer.sizeLessThan(8))
      Attempt.failure(Err.insufficientBits(8, buffer.size))
    else
      decode(bits, buffer.bytes).map(_.map(_.toInt)).flatMap {
        case r @ DecodeResult(i, _) =>
          if (i >= 0)
            Attempt.successful(r)
          else
            Attempt.failure(Err("invalid unsigned int"))
      }

  private def decode(n: Int, buffer: ByteVector): Attempt[DecodeResult[Long]] =
    if (n <= 0) {
      Attempt.failure(Err("integer representation too long"))
    } else if (buffer.isEmpty) {
      Attempt.failure(Err("unexpected end of input"))
    } else {
      val byte = buffer.head
      if (n >= 7 || (byte & 0x7f) < (1 << n)) {
        val x = byte & 0X7FL
        if ((byte & 0x80) == 0)
          Attempt.successful(DecodeResult(x, buffer.tail.bits))
        else
          decode(n - 7, buffer.tail).map(_.map(s => x | (s << 7)))
      } else {
        Attempt.failure(Err("integer too large"))
      }
    }

  def encode(value: Int): Attempt[BitVector] =
    if (value > MaxValue)
      Attempt.failure(Err(s"$value is greater than maximum value $MaxValue for $description"))
    else if (value < MinValue)
      Attempt.failure(Err(s"$value is less than minimum value $MinValue for $description"))
    else
      Attempt.successful(encode(value, ByteVector.empty))

  @tailrec
  private def encode(value: Int, buffer: ByteVector): BitVector = {
    val byte = (value & 0x7f).toByte
    val value1 = value >> 7
    val byte1 =
      if (value1 != 0)
        (byte | 0x80).toByte
      else
        byte
    val buffer1 = buffer :+ byte1
    if (value1 == 0)
      buffer1.bits
    else
      encode(value1, buffer1)
  }

  private val maxBits = (bitsL / 7) + (if (bitsL % 7 == 0) 0 else 1)

  def sizeBound: SizeBound =
    SizeBound.bounded(8L, maxBits)

}

private class Varint(bits: Int) extends Codec[Long] {

  private val bitsL = bits.toLong

  val MaxValue = (1 << (bits - 1)) - 1
  val MinValue = -(1 << (bits - 1))

  private def description = s"$bits-bit signed leb128 integer"

  def decode(buffer: BitVector): Attempt[DecodeResult[Long]] =
    if (buffer.sizeLessThan(8))
      Attempt.failure(Err.insufficientBits(8, buffer.size))
    else
      decode(bits, buffer.bytes)

  private def decode(n: Int, buffer: ByteVector): Attempt[DecodeResult[Long]] =
    if (n <= 0) {
      Attempt.failure(Err("integer representation too long"))
    } else if (buffer.isEmpty) {
      Attempt.failure(Err("unexpected end of input"))
    } else {
      val byte = buffer.head
      val mask = (-1 << (n - 1)) & 0x7f
      if (n >= 7 || (byte & mask) == 0 || (byte & mask) == mask) {
        val x = byte & 0X7FL
        if ((byte & 0x80) == 0)
          if ((byte & 0x40) == 0)
            Attempt.successful(DecodeResult(x, buffer.tail.bits))
          else
            Attempt.successful(DecodeResult(x ^ ((-1L) ^ 0X7FL), buffer.tail.bits))
        else
          decode(n - 7, buffer.tail).map(s => s.map(s => x | (s << 7)))
      } else {
        Attempt.failure(Err("integer too large"))
      }
    }

  def encode(value: Long): Attempt[BitVector] =
    if (value > MaxValue)
      Attempt.failure(Err(s"$value is greater than maximum value $MaxValue for $description"))
    else if (value < MinValue)
      Attempt.failure(Err(s"$value is less than minimum value $MinValue for $description"))
    else
      Attempt.successful(encode(value, true, ByteVector.empty))

  @tailrec
  private def encode(value: Long, more: Boolean, buffer: ByteVector): BitVector =
    if (more) {
      val byte = (value & 0X7FL).toByte
      val value1 = value >> 7

      val (byte1, more1) =
        if ((value1 == 0 && (byte & 0x40) == 0x00) || (value1 == -1 && (byte & 0x40) == 0x40))
          (byte, false)
        else
          ((byte | 0x80).toByte, true)
      val buffer1 = buffer :+ byte1
      encode(value1, more1, buffer1)
    } else {
      buffer.bits
    }

  private val maxBits = (bitsL / 7) + (if (bitsL % 7 == 0) 0 else 1)

  def sizeBound: SizeBound =
    SizeBound.bounded(8L, maxBits)

}
