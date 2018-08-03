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

import cats._

import scala.language.higherKinds

package object imports {

  def NoImports[F[_]]: Imports[F] =
    new Imports[F](TCMap.empty[String, AsInstance[?, F]])

  def module[T, F[_]](name: String, mod: T)(implicit I: AsInstance[T, F]): (String, Elem[AsInstance[?, F]]) =
    (name, Elem.fromValue[T, AsInstance[?, F]](mod))

  implicit def instanceAsInstance[F[_]]: AsInstance[Instance[F], F] =
    new AsInstance[Instance[F], F] {
      def find(i: Instance[F], field: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
        i.exports.field(field)
    }

  implicit def tcMapAsInstance[F[_]]: AsInstance[TCMap[String, AsInterface[?, F]], F] =
    new AsInstance[TCMap[String, AsInterface[?, F]], F] {
      def find(m: TCMap[String, AsInterface[?, F]], field: String)(
          implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
        m.get(field) match {
          case Some(elem) => F.pure(elem.typeclass.view(elem.value))
          case None       => F.raiseError(new RuntimeException(s"Unknown field $field"))
        }
    }

  implicit def interfaceAsInterface[F[_]]: AsInterface[Interface[F, Type], F] =
    new AsInterface[Interface[F, Type], F] {
      def view(i: Interface[F, Type]) = i
    }

  implicit def valueAsInterface[T, F[_]](implicit writer: ValueWriter[T]): AsInterface[T, F] =
    new AsInterface[T, F] {
      def view(t: T): Global[F] =
        new Global[F] {
          val tpe = GlobalType(writer.swamType, Mut.Const)
          def get = writer.write(t)
        }
    }

  implicit def procedure0AsInterface[F[_]](implicit F: MonadError[F, Throwable]): AsInterface[() => F[Unit], F] =
    new AsInterface[() => F[Unit], F] {
      def view(f: () => F[Unit]) = new IFunction0Unit[F](f)
    }

  implicit def function0AsInsterface[Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                writer: ValueWriter[Ret]): AsInterface[() => F[Ret], F] =
    new AsInterface[() => F[Ret], F] {
      def view(f: () => F[Ret]) = new IFunction0(f)
    }

  implicit def procedure1AsInterface[P1, F[_]](implicit F: MonadError[F, Throwable],
                                               reader1: ValueReader[P1]): AsInterface[(P1) => F[Unit], F] =
    new AsInterface[(P1) => F[Unit], F] {
      def view(f: (P1) => F[Unit]) = new IFunction1Unit[F, P1](f)
    }

  implicit def function1AsInterface[P1, Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                   reader1: ValueReader[P1],
                                                   writer: ValueWriter[Ret]): AsInterface[(P1) => F[Ret], F] =
    new AsInterface[(P1) => F[Ret], F] {
      def view(f: (P1) => F[Ret]) = new IFunction1[F, P1, Ret](f)
    }

  implicit def procedure2AsInterface[P1, P2, F[_]](implicit F: MonadError[F, Throwable],
                                                   reader1: ValueReader[P1],
                                                   reader2: ValueReader[P2]): AsInterface[(P1, P2) => F[Unit], F] =
    new AsInterface[(P1, P2) => F[Unit], F] {
      def view(f: (P1, P2) => F[Unit]) = new IFunction2Unit[F, P1, P2](f)
    }

  implicit def function2AsInterface[P1, P2, Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                       reader1: ValueReader[P1],
                                                       reader2: ValueReader[P2],
                                                       writer: ValueWriter[Ret]): AsInterface[(P1, P2) => F[Ret], F] =
    new AsInterface[(P1, P2) => F[Ret], F] {
      def view(f: (P1, P2) => F[Ret]) = new IFunction2[F, P1, P2, Ret](f)
    }

  implicit def arrayAsInterface[F[_]]: AsInterface[Array[Function[F]], F] =
    new AsInterface[Array[Function[F]], F] {
      def view(a: Array[Function[F]]) = new Table[F] {
        def tpe = TableType(ElemType.AnyFunc, Limits(size, Some(size)))
        def size =
          a.length
        def apply(idx: Int) =
          a(idx)
        def update(idx: Int, f: Function[F]) =
          a(idx) = f
      }
    }

}
