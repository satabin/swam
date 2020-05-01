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

private[wasi] trait ArgsOps[F[_]] extends WasiBase[F] {

  def argsGet(argv: Pointer, argvBuf: Pointer): F[Errno] =
    args
      .foldLeftM((argv, argvBuf)) {
        case ((coffset, offset), arg) =>
          val argBytes = arg.getBytes("UTF-8")
          for {
            mem <- mem.get
            _ <- mem.writeInt(coffset, offset)
            _ <- mem.writeBytes(offset, argBytes)
            _ <- mem.writeByte(offset + argBytes.length, 0)
          } yield (coffset + 4, offset + argBytes.length + 1)
      }
      .as(success)
      .handleErrorWith(t => logger.error("could not get the arguments", t).as(io))

  def argsSizesGet(argc: Pointer, argvBufSize: Pointer): F[Errno] =
    (for {
      mem <- mem.get
      _ <- mem.writeInt(argc, args.length)
      _ <- mem.writeInt(argvBufSize, args.map(t => t.length + 1).sum)
    } yield success)
      .handleErrorWith(t => logger.error("could not get the argument sizes", t).as(io))

}
