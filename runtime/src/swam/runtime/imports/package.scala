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

import cats._

import scala.language.higherKinds

package object imports {

  implicit def mapImports[F[_]]: Imports[Map[String, Map[String, Interface[F, Type]]], F] =
    new Imports[Map[String, Map[String, Interface[F, Type]]], F] {
      def find(map: Map[String, Map[String, Interface[F, Type]]], module: String, field: String)(
          implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
        map.get(module) match {
          case Some(fields) =>
            fields.get(field) match {
              case Some(i) => F.pure(i)
              case None    => F.raiseError(new RuntimeException(s"Unknown field $field in imported module $module"))
            }
          case None => F.raiseError(new RuntimeException(s"Unknown imported module $module"))
        }
    }

}
