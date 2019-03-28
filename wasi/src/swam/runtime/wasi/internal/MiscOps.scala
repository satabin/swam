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

import java.security.SecureRandom

private[wasi] trait MiscOps[F[_]] extends WasiBase[F] {

  private val random = SecureRandom.getInstanceStrong()

  def pollOneoff(in: Pointer, out: Pointer, nsubscriptions: Size, nevents: Pointer): F[Errno] =
    unimplemented("poll_oneoff")

  def procExit(rval: Exitcode): F[Unit] =
    F.delay(sys.exit(rval))

  def procRaise(sig: Signal): F[Errno] =
    F.delay(sun.misc.Signal.raise(new sun.misc.Signal(sig.entryName))).as(success)

  def schedYield(): F[Errno] =
    contextShift.shift.as(success)

  def randomGet(buf: Pointer, bufLen: Size): F[Errno] = {
    val bytes = Array.ofDim[Byte](bufLen)
    (for {
      _ <- blocker.delay(random.nextBytes(bytes))
      mem <- mem.get
      _ <- mem.writeBytes(buf, bytes)
    } yield success).handleErrorWith(t => logger.error(s"unable to get $bufLen random bytes", t).as(io))
  }

}
