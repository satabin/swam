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

sealed trait Value
object Value {
  case class Int32(i: Int) extends Value
  case class Int64(l: Long) extends Value
  case class Float32(f: Float) extends Value
  case class Float64(d: Double) extends Value

  def zero(tpe: ValType): Value = tpe match {
    case ValType.I32 => Int32(0)
    case ValType.I64 => Int64(0l)
    case ValType.F32 => Float32(0.0f)
    case ValType.F64 => Float64(0.0d)
  }

  def tpe(v: Value): ValType =
    v match {
      case Int32(_) => ValType.I32
      case Int64(_) => ValType.I64
      case Float32(_) => ValType.F32
      case Float64(_) => ValType.F64
    }
}
