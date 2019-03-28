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

import java.util.concurrent.TimeUnit

private[wasi] trait ClockOps[F[_]] extends WasiBase[F] {

  // Note: assumes nanosecond precision is available but it depends on the `Clock` implementation
  // and the underlying platform
  def clockResGet(id: Clockid, resolution: Pointer): F[Errno] =
    id match {
      case Clockid.Monotonic =>
        mem.get.flatMap { mem =>
          mem
            .writeLong(resolution, 1L)
            .as(success)
            .handleErrorWith(t => logger.error("could not get the clock resolution", t).as(io))
        }
      case Clockid.Realtime =>
        mem.get.flatMap { mem =>
          mem
            .writeLong(resolution, 1000000L)
            .as(success)
            .handleErrorWith(t => logger.error("could not get the clock resolution", t).as(io))
        }
      case _ =>
        F.pure(inval)
    }

  def clockTimeGet(id: Clockid, precision: Timestamp, time: Pointer): F[Errno] =
    id match {
      case Clockid.Monotonic =>
        (for {
          nanos <- clock.monotonic(TimeUnit.NANOSECONDS)
          mem <- mem.get
          _ <- mem.writeLong(time, nanos)
        } yield success)
          .handleErrorWith(t => logger.error("could not get the clock value", t).as(io))
      case Clockid.Realtime =>
        (for {
          millis <- clock.realTime(TimeUnit.MILLISECONDS)
          mem <- mem.get
          _ <- mem.writeLong(time, millis)
        } yield success)
          .handleErrorWith(t => logger.error("could not get the clock value", t).as(io))
      case _ =>
        F.pure(inval)
    }

}
