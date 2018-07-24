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

import formats._
import functions._

import scala.language.higherKinds

sealed trait ImportedModule[F[_]]

case class SwamModule[F[_]](module: Module[F]) extends ImportedModule[F]

case class ScalaModule[F[_]](val fields: Map[String, ScalaImport[F]]) extends ImportedModule[F]

sealed trait ScalaImport[F[_]]

class ScalaFunction[F[_]](val f: Importable[F]) extends ScalaImport[F]

class ScalaVar[T, F[_]](private var _value: T)(implicit format: ValueFormatter[T]) extends ScalaImport[F] {

  def tpe: ValType = format.swamType

  def apply(): T = _value

  def value: T = _value

  def update(v: T): Unit = _value = v

  def :=(v: T): Unit = _value = v

}
