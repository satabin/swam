/*
 * Copyright 2020 Lucas Satabin
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
package wasi
package internal

import cats.implicits._

private[wasi] trait EnvironmentOps[F[_]] extends WasiBase[F] {

  def environGet(environ: Pointer, buf: Pointer): F[Errno] =
    sys.env.toList
      .foldLeftM((environ, buf)) {
        case ((coffset, offset), (name, value)) =>
          val bytes = s"$name=$value".getBytes("UTF-8")
          for {
            mem <- mem.get
            _ <- mem.writeInt(coffset, offset)
            _ <- mem.writeBytes(offset, bytes)
            _ <- mem.writeByte(offset + bytes.length, 0)
          } yield (coffset + 4, offset + bytes.length + 1)
      }
      .as(success)
      .handleErrorWith(t => logger.error("could not get the environment", t).as(io))

  def environSizesGet(environc: Pointer, environBufSize: Pointer): F[Errno] = {
    val (size, count) =
      sys.env
        .foldLeft((0, 0)) {
          case ((cumul, count), (name, value)) =>
            val entrySize = name.length + value.length + 2
            (cumul + entrySize, count + 1)
        }

    (for {
      mem <- mem.get
      _ <- mem.writeInt(environc, count)
      _ <- mem.writeInt(environBufSize, size)
    } yield success)
      .handleErrorWith(t => logger.error("could not get the environment size", t).as(io))
  }

}
