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

import pretty._

import cats._
import cats.data._
import cats.Show._
import cats.effect._
import cats.implicits._

import scala.language.higherKinds

/** This code is inspired by https://github.com/vlovgr/io-and-logging-capabilities
  * described in https://blog.vlovgr.se/post/2018-09-24-io-and-logging-capabilities/.
  */
package object logging {

  type Log[F[_], A, E] = WriterT[F, Chain[E], A]

  implicit class EvalFormatString(private val sc: StringContext) extends AnyVal {

    /** Similar to the cats `show` interpolator but in an eval.
      * This is used to avoid string creation if nothing is logged.
      */
    def eval(args: Shown*): Eval[String] =
      Eval.later(sc.s(args: _*))

  }

  /** Determines the log level of several log `entries`.
    * The log level is the _highest_ of all entries, i.e. the smallest one.
    */
  def logLevel[G[_]](entries: G[LogEntry])(implicit G: Foldable[G]): Option[LogLevel] =
    entries.minimumOption.map(_.level)

  /** Creates the log message for accumulated `entries`.
    * If there are no entries, returns `None`, otherwise return
    * an `Eval` of [[pretty.Doc Doc]] that will be run when logging
    * is actually performed.
    *
    * Rendered entries are enclosed between `open` and `close` documents and
    * separated by `sep`.
    *
    * If `entries` is empty, then returns the empty document.
    */
  def logMessage[G[_]](open: Doc, sep: Doc, close: Doc)(entries: G[LogEntry])(implicit G: Foldable[G]): Eval[Doc] =
    entries
      .foldM(empty) {
        case (`empty`, entry) => entry.message.map(str(_))
        case (prefix, entry)  => entry.message.map(m => prefix ++ sep ++ str(m))
      }
      .map {
        case `empty` => empty
        case d       => open ++ d ++ close
      }

  implicit def logMonadLog[F[_], E](implicit A: Monad[Log[F, ?, E]], F: Sync[F]): MonadLog[Log[F, ?, E], Chain, E] =
    new MonadLog[Log[F, ?, E], Chain, E] {
      override val monad: Monad[Log[F, ?, E]] = A
      override val applicative: Applicative[Chain] = implicitly
      override val monoidK: MonoidK[Chain] = implicitly
      override def log(e: E): Log[F, Unit, E] = WriterT.tell(Chain.one(e))
      override def clear[A](fa: Log[F, A, E]): Log[F, A, E] = fa.mapWritten(_ => Chain.empty)
      override def extract[A](fa: Log[F, A, E]): Log[F, (A, Chain[E]), E] = fa.mapBoth((ce, a) => (ce, (a, ce)))
    }
}
