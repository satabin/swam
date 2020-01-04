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

import cats._
import cats.implicits._

abstract class EFunction0[Ret, F[_]] private (f: Function[F], m: Option[Memory[F]])(
    implicit F: MonadError[F, Throwable])
    extends EFunction[Ret, F]
    with Function0[F[Ret]] {
  def apply(): F[Ret] =
    f.invoke(Vector(), m).flatMap(wrap(_))
}

private[runtime] object EFunction0 {

  def apply[F[_]](name: String, self: Instance[F])(implicit F: MonadError[F, Throwable]): F[EFunction0[Unit, F]] =
    self.exps.get(name) match {
      case Some(f: Function[F]) =>
        f.tpe match {
          case FuncType(Vector(), Vector()) =>
            F.pure(new EFunction0[Unit, F](f, self.memories.headOption) {
              def wrap(res: Option[Value]): F[Unit] = EFunction.wrapUnit[F](res)
            })
          case functype =>
            F.raiseError(new RuntimeException(s"invalid function type (expected () => Unit but got $functype)"))
        }
      case Some(fld) =>
        F.raiseError(new RuntimeException(s"cannot get a function for type ${fld.tpe}"))
      case None =>
        F.raiseError(new RuntimeException(s"unknown function named $name"))
    }

  def apply[Ret, F[_]](name: String, self: Instance[F])(implicit F: MonadError[F, Throwable],
                                                        reader: ValueReader[F, Ret]): F[EFunction0[Ret, F]] =
    self.exps.get(name) match {
      case Some(f: Function[F]) =>
        f.tpe match {
          case FuncType(Vector(), Vector(reader.swamType)) =>
            F.pure(new EFunction0[Ret, F](f, self.memories.headOption) {
              def wrap(res: Option[Value]): F[Ret] = EFunction.wrap[F, Ret](res, self.memories.headOption)
            })
          case functype =>
            F.raiseError(new RuntimeException(s"invalid function type (expected () => Unit but got $functype)"))
        }
      case Some(fld) =>
        F.raiseError(new RuntimeException(s"cannot get a function for type ${fld.tpe}"))
      case None =>
        F.raiseError(new RuntimeException(s"unknown function named $name"))
    }

}
