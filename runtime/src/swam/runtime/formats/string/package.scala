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
package runtime
package formats

import cats._

import scala.collection.mutable.ArrayBuilder

import scala.language.higherKinds

package object string {

  /** Decodes a null-terminated ASCII string. */
  implicit object cstring extends ValueReader[String] {
    val swamType: ValType = ValType.I32
    def read[F[_]](v: Value, m: Option[Memory[F]])(implicit F: MonadError[F, Throwable]): F[String] =
      (v, m) match {
        case (Value.Int32(i), Some(m)) =>
          val size = m.size
          val bytes = ArrayBuilder.make[Byte]
          F.tailRecM(i) { i =>
            if (i >= size) {
              F.raiseError(new RuntimeException("non terminated string"))
            } else if (m.readByte(i) == 0) {
              F.pure(Right(new String(bytes.result, "ASCII")))
            } else {
              bytes += m.readByte(i)
              F.pure(Left(i + 1))
            }
          }
        case (_, Some(_)) => F.raiseError(new RuntimeException(s"expected i32 but got ${v.tpe}"))
        case (_, None)    => F.raiseError(new RuntimeException("missing memory"))
      }
  }

  /** Decodes a UTF-8 encoed string starting with its size in bytes
    * encoded in little-endian on 4 bytes.
    */
  implicit object utf8 extends ValueReader[String] {
    val swamType: ValType = ValType.I32
    def read[F[_]](v: Value, m: Option[Memory[F]])(implicit F: MonadError[F, Throwable]): F[String] =
      (v, m) match {
        case (Value.Int32(idx), Some(m)) =>
          val msize = m.size
          if (idx >= msize - 4) {
            F.raiseError(new RuntimeException("out of bound memory access"))
          } else {
            val ssize = m.readInt(idx)
            println(ssize)
            if ((idx + 4 + ssize) > msize) {
              F.raiseError(new RuntimeException("out of bound memory access"))
            } else {
              val bytes = ArrayBuilder.make[Byte]
              F.tailRecM(0) { i =>
                if (i >= ssize) {
                  F.pure(Right(new String(bytes.result, "UTF8")))
                } else {
                  bytes += m.readByte(idx + 4 + i)
                  F.pure(Left(i + 1))
                }
              }
            }
          }
        case (_, Some(_)) => F.raiseError(new RuntimeException(s"expected i32 but got ${v.tpe}"))
        case (_, None)    => F.raiseError(new RuntimeException("missing memory"))
      }
  }

}
