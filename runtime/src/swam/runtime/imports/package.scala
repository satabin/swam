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

import cats._

import scala.language.higherKinds

package object imports {

  def NoImports[F[_]]: Imports[F] =
    new Imports[F](TCMap.empty[String, AsInstance[?, F]])

  def module[T, F[_]](name: String, mod: T)(implicit I: AsInstance[T, F]): (String, Elem[AsInstance[?, F]]) =
    (name, Elem.fromValue[T, AsInstance[?, F]](mod))

  implicit def instanceAsInstance[F[_]]: AsInstance[Instance[F], F] =
    new AsInstance[Instance[F], F] {
      def find(i: Instance[F], field: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] = i.exports.field(field)
    }

  implicit def tcMapAsInstance[F[_]]: AsInstance[TCMap[String, AsInterface[?, F]], F] =
    new AsInstance[TCMap[String, AsInterface[?, F]], F] {
      def find(m: TCMap[String, AsInterface[?, F]], field: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
        m.get(field) match {
          case Some(elem) => F.pure(elem.typeclass.view(elem.value))
          case None => F.raiseError(new RuntimeException(s"Unknown field $field"))
      }
    }

  implicit def interfaceAsInterface[F[_]]: AsInterface[Interface[F, Type], F] =
    new AsInterface[Interface[F, Type], F] {
      def view(i: Interface[F, Type]) = i
    }

  implicit def intAsInterface[T, F[_]](implicit writer: ValueWriter[T]): AsInterface[T, F] =
    new AsInterface[T, F] {
      def view(t: T): Global[F] =
        new Global[F] {
          val tpe = GlobalType(writer.swamType, Mut.Const)
          def get = writer.write(t)
        }
    }

    implicit def procedure0AsInterface[F[_]](implicit F: MonadError[F, Throwable]): AsInterface[() => F[Unit], F] = new AsInterface[() => F[Unit], F] {
      def view(f: () => F[Unit]) = new IFunction0Unit[F](f)
    }

    implicit def function0AsInsterface[Ret, F[_]](implicit F: MonadError[F, Throwable], writer: ValueWriter[Ret]): AsInterface[() => F[Ret], F] = new AsInterface[() => F[Ret], F] {
      def view(f: () => F[Ret]) = new IFunction0(f)
    }

}
