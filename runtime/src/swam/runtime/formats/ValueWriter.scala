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

/** A writer is used to transform a scala value into a
  *  web assembly value.
  */
trait ValueWriter[F[_], T] {

  def write(v: T, m: Option[Memory[F]]): F[Value]

  val swamType: ValType

}

/** A writer is used to transform a scala value into a
  *  simple web assembly value. A simple value can be writter without memory instance.
  */
abstract class SimpleValueWriter[F[_], T](implicit F: MonadError[F, Throwable]) extends ValueWriter[F, T] {

  @inline
  final override def write(v: T, m: Option[Memory[F]]): F[Value] =
    F.pure(write(v))

  def write(v: T): Value

}
