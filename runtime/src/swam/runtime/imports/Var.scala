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
package imports

import formats._

import cats._

import scala.language.higherKinds

/** An imported variable wrapper.
  *  Makes it possible to pass a mutable global from Scala to Swam.
  */
class Var[F[_], T] private (private var _value: Value)(implicit F: MonadError[F, Throwable], formatter: ValueFormatter[T])
    extends Global[F] {

  val tpe = GlobalType(formatter.swamType, Mut.Var)

  def get: Value =
    _value

  def apply(): F[T] =
    formatter.read[F](get)

  def value: F[T] =
    apply()

  def value_=(v: T): Unit =
    set(formatter.write(v))

  def set(v: Value): Unit =
    _value = v

  def :=(v: T): Unit =
    value = v

}

object Var {

  def apply[F[_], T](v: T)(implicit F: MonadError[F, Throwable], formatter: ValueFormatter[T]): Var[F, T] =
    new Var(formatter.write(v))

}
