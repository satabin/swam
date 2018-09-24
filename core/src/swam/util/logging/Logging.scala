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

package swam.util
package logging

import pretty._

import cats._
import cats.implicits._

import scala.language.higherKinds

abstract class Logging[F[_], G[_]] {

  def log(entry: LogEntry): F[Unit]

  def logNow(entry: LogEntry, mdc: G[MdcEntry]): F[Unit]

  def logNow(entry: LogEntry): F[Unit]

  def dispatchLogs[A](fa: F[A], mdc: G[MdcEntry]): F[A]

  def dispatchLogs[A](fa: F[A]): F[A]

  def extractLogs[A](fa: F[A]): F[(A, G[LogEntry])]

}

object Logging {
  def create[F[_], G[_]](open: Doc, sep: Doc, close: Doc, dispatch: (Eval[Doc], G[MdcEntry], LogLevel) => F[Unit])(
      implicit F: MonadLog[F, G, LogEntry],
      G: Foldable[G],
      M: MonoidK[G]): Logging[F, G] =
    new Logging[F, G] {

      override def log(entry: LogEntry): F[Unit] =
        F.log(entry)

      override def logNow(entry: LogEntry, mdc: G[MdcEntry]): F[Unit] =
        dispatchLogs(log(entry), mdc)

      override def logNow(entry: LogEntry): F[Unit] =
        logNow(entry, M.empty)

      override def dispatchLogs[A](fa: F[A], mdc: G[MdcEntry]): F[A] =
        F.flush(fa) { entries =>
          (Some(logMessage(open, sep, close)(entries)), Some(mdc), logLevel(entries))
            .mapN(dispatch)
            .getOrElse(F.monad.unit)
        }

      override def dispatchLogs[A](fa: F[A]): F[A] =
        dispatchLogs(fa, M.empty)

      override def extractLogs[A](fa: F[A]): F[(A, G[LogEntry])] =
        F.extract(fa)
    }
}
