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
import cats.implicits._

class IFunction1Unit[F[_], P1](f: (P1) => F[Unit])(implicit reader1: ValueReader[F, P1], F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType), Vector())
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1) =>
        for {
          p1 <- reader1.read(p1, m)
          _ <- f(p1)
        } yield None
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction1[F[_], P1, Ret](f: (P1) => F[Ret])(implicit reader1: ValueReader[F, P1],
                                                   writer: ValueWriter[F, Ret],
                                                   F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType), Vector(writer.swamType))
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1) =>
        for {
          p1 <- reader1.read(p1, m)
          v <- f(p1)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}
