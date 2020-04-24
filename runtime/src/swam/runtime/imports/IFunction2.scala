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

class IFunction2[F[_], P1, P2, Ret](f: (P1, P2) => F[Ret])(implicit reader1: ValueReader[F, P1],
                                                           reader2: ValueReader[F, P2],
                                                           writer: FunctionReturnWriter[F, Ret],
                                                           F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType), writer.swamTypes)
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Vector[Value]] =
    parameters match {
      case Seq(p1, p2) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          v <- f(p1, p2)
          v <- writer.write(v, m)
        } yield v
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}
