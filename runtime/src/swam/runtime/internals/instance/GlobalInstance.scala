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

import cats._

private[runtime] class GlobalInstance[F[_]](val tpe: GlobalType)(implicit F: MonadError[F, Throwable])
    extends Global[F] {

  private var raw: Long = 0L

  def get: Value =
    Value.fromRaw(tpe.tpe, raw)

  def set(v: Value) =
    if (tpe.mut == Mut.Var) {
      if (v.tpe <:< tpe.tpe) {
        F.pure { raw = Value.toRaw(v) }
      } else {
        F.raiseError(new RuntimeException(s"Expected type ${tpe.tpe} but got ${v.tpe}"))
      }
    } else {
      F.raiseError(new RuntimeException("Unable to set value to immutable global"))
    }

  private[runtime] def rawget: Long =
    raw

  private[runtime] def rawset(v: Long) =
    raw = v

}
