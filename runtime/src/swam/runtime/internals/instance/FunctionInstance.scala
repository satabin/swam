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
package internals
package instance

import interpreter._

import cats._
import cats.implicits._

private[runtime] case class FunctionInstance[F[_]](tpe: FuncType,
                                                   locals: Vector[ValType],
                                                   code: Array[AsmInst[F]],
                                                   instance: Instance[F],
                                                   name: Option[String])(implicit F: MonadError[F, Throwable])
    extends Function[F] {

  var next: FunctionInstance[F] = _

  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Vector[Value]] =
    instance.interpreter
      .interpret(this, parameters.map(Value.toRaw(_)), instance)
      .map(_.zip(tpe.t).map { case (v, t) => Value.fromRaw(t, v) })

}
