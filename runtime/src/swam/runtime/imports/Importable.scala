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

import cats._

import scala.language.higherKinds

  /** Represents the imported elements of an instance.
    *  These can either be other module [[Instance]]s or Scala
    *  functions and variables made available to interact between
    *  both worlds.
    */
trait Imports[T, F[_]] {

  def find(t: T, module: String, field: String)(implicit F: MonadError[F, Throwable]): F[Importable[F]]

}

/** A field that can be imported. */
trait Importable[F[_]] {
  def tpe: Type
}
