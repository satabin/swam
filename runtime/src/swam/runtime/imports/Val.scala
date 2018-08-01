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

/** An imported value wrapper.
  *  Makes it possible to pass a global from Scala to Swam.
  */
class Val[F[_], T](v: T)(implicit F: MonadError[F, Throwable], writer: ValueWriter[T]) extends Global[F] {

  val tpe = GlobalType(writer.swamType, Mut.Const)

  def get: Value =
    writer.write(v)

  def set(v: Value) =
    throw new UnsupportedOperationException("Val.set")

  def value: Value =
    get

}
