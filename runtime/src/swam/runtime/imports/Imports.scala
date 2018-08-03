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
class Imports[F[_]](imports: TCMap[String, AsInstance[?, F]]) {

  def find(module: String, field: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
    imports.get(module) match {
      case Some(elem) => elem.typeclass.find(elem.value, field)
      case None =>
        F.raiseError(new RuntimeException(s"Unknown module $module"))
    }

}

object Imports {

  def apply[F[_]](imported: (String, Elem[AsInstance[?, F]])*): Imports[F] =
    new Imports[F](TCMap[String, AsInstance[?, F]](imported: _*))

}

/** A typeclass that describes what it means for an imported object to be viewed as
  *  an instance from the engine.
  */
trait AsInstance[T, F[_]] {

  def find(t: T, field: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]]

}

/** A typeclass that describes what it means for a type to be viewed as
  *  an interface element from the engine.
  */
trait AsInterface[T, F[_]] {

  def view(v: T): Interface[F, Type]

}
