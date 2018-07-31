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
package exports

import formats._
import internals.instance._

import cats._

import scala.language.higherKinds

/** An exported global value wrapper.
 *  Makes it possible to interact with global immutable values from Scala.
 */
class Val[F[_], T](global: GlobalInstance[F])(implicit F: MonadError[F, Throwable], format: ValueReader[T]) {

  def apply(): F[T] =
    format.read[F](global.value)

  def value: F[T] =
    apply()

}
