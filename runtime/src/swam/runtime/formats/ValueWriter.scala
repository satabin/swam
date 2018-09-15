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

import scala.language.higherKinds

/** A writer is used to transform a scala value into a
  *  web assembly value.
  */
trait ValueWriter[T] {

  def write[F[_]](v: T, m: Option[Memory[F]])(implicit F: MonadError[F, Throwable]): F[Value]

  val swamType: ValType

}

/** A writer is used to transform a scala value into a
  *  simple web assembly value. A simple value can be writter without memory instance.
  */
trait SimpleValueWriter[T] extends ValueWriter[T] {

  @inline
  final override def write[F[_]](v: T, m: Option[Memory[F]])(implicit F: MonadError[F, Throwable]): F[Value] =
    F.pure(write(v))

  def write(v: T): Value

}
