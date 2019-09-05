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

abstract class EFunction1[P1, Ret, F[_]] private (f: Function[F], m: Option[Memory[F]])(
    implicit F: MonadError[F, Throwable],
    writer1: ValueWriter[F, P1])
    extends EFunction[Ret, F]
    with Function1[P1, F[Ret]] {
  def apply(p1: P1): F[Ret] =
    for {
      p1 <- writer1.write(p1, m)
      v <- f.invoke(Vector(p1), m)
      v <- wrap(v)
    } yield v
}

object EFunction1 {

  def apply[P1, F[_]](name: String, self: Instance[F])(implicit F: MonadError[F, Throwable],
                                                       writer1: ValueWriter[F, P1]): F[EFunction1[P1, Unit, F]] =
    self.exps.get(name) match {
      case Some(f: Function[F]) =>
        val expectedt = FuncType(Vector(writer1.swamType), Vector())
        if (f.tpe == expectedt)
          F.pure(new EFunction1[P1, Unit, F](f, self.memories.headOption) {
            def wrap(res: Option[Value]): F[Unit] = EFunction.wrapUnit[F](res)
          })
        else
          F.raiseError(new RuntimeException(s"invalid return type (expected $expectedt but got ${f.tpe}"))
      case Some(fld) =>
        F.raiseError(new RuntimeException(s"cannot get a function for type ${fld.tpe}"))
      case None =>
        F.raiseError(new RuntimeException(s"unknown function named $name"))
    }

  def apply[P1, Ret, F[_]](name: String, self: Instance[F])(implicit F: MonadError[F, Throwable],
                                                            writer1: ValueWriter[F, P1],
                                                            reader: ValueReader[F, Ret]): F[EFunction1[P1, Ret, F]] =
    self.exps.get(name) match {
      case Some(f: Function[F]) =>
        val expectedt = FuncType(Vector(writer1.swamType), Vector(reader.swamType))
        if (f.tpe == expectedt)
          F.pure(new EFunction1[P1, Ret, F](f, self.memories.headOption) {
            def wrap(res: Option[Value]): F[Ret] = EFunction.wrap[F, Ret](res, self.memories.headOption)
          })
        else
          F.raiseError(new RuntimeException(s"invalid return type (expected $expectedt but got ${f.tpe}"))
      case Some(fld) =>
        F.raiseError(new RuntimeException(s"cannot get a function for type ${fld.tpe}"))
      case None =>
        F.raiseError(new RuntimeException(s"unknown function named $name"))
    }

}
