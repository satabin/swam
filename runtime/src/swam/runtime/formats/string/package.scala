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
import cats.implicits._

import scala.collection.mutable.ArrayBuilder

package object string {

  /** Decodes a null-terminated ASCII string. */
  implicit def cstring[F[_]](implicit F: MonadError[F, Throwable]): ValueReader[F, String] =
    new ValueReader[F, String] {
      val swamType: ValType = ValType.I32
      def read(v: Value, m: Option[Memory[F]]): F[String] =
        (v, m) match {
          case (Value.Int32(i), Some(m)) =>
            val size = m.size
            val bytes = ArrayBuilder.make[Byte]
            F.tailRecM(i) { i =>
              if (i >= size)
                F.raiseError(new RuntimeException("non terminated string"))
              else
                m.readByte(i).flatMap { b =>
                  if (b == 0) {
                    F.pure(Right(new String(bytes.result, "ASCII")))
                  } else {
                    m.readByte(i).map { b =>
                      bytes += b
                      Left(i + 1)
                    }
                  }
                }
            }
          case (_, Some(_)) => F.raiseError(new RuntimeException(s"expected i32 but got ${v.tpe}"))
          case (_, None)    => F.raiseError(new RuntimeException("missing memory"))
        }
    }

  /** Decodes a UTF-8 encoed string starting with its size in bytes
    * encoded in little-endian on 4 bytes.
    */
  implicit def utf8[F[_]](implicit F: MonadError[F, Throwable]): ValueReader[F, String] = new ValueReader[F, String] {
    val swamType: ValType = ValType.I32
    def read(v: Value, m: Option[Memory[F]]): F[String] =
      (v, m) match {
        case (Value.Int32(idx), Some(m)) =>
          val msize = m.size
          if (idx >= msize - 4)
            F.raiseError(new RuntimeException("out of bound memory access"))
          else
            m.readInt(idx).flatMap { ssize =>
              if ((idx + 4 + ssize) > msize) {
                F.raiseError(new RuntimeException("out of bound memory access"))
              } else {
                val bytes = ArrayBuilder.make[Byte]
                F.tailRecM(0) { i =>
                  if (i >= ssize)
                    F.pure(Right(new String(bytes.result, "UTF8")))
                  else
                    m.readByte(idx + 4 + i).map { b =>
                      bytes += b
                      Left(i + 1)
                    }
                }
              }
            }
        case (_, Some(_)) => F.raiseError(new RuntimeException(s"expected i32 but got ${v.tpe}"))
        case (_, None)    => F.raiseError(new RuntimeException("missing memory"))
      }
  }

}
