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

package swam.util.logging

import cats.{Applicative, Monad, MonoidK}

import scala.language.higherKinds

trait MonadLog[F[_], G[_], E] {

  val monad: Monad[F]

  val applicative: Applicative[G]

  val monoidK: MonoidK[G]

  import monad.{as, flatMap}

  /** Appends a log entry. It is accumulated but not logged yet. */
  def log(e: E): F[Unit]

  /** Clears the currently collected log entries. */
  def clear[A](fa: F[A]): F[A]

  /** Extracts the currently collected log entries to do actual logging. */
  def extract[A](fa: F[A]): F[(A, G[E])]

  /** [[extract Extracts]] and then [[clear clears]] the log entries. */
  def flush[A](fa: F[A])(f: G[E] => F[Unit]): F[A] =
    clear(flatMap(extract(fa)) { case (a, ge) => as(f(ge), a) })

}
