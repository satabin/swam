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

sealed trait Type

sealed trait ValType extends Type

object ValType {
  case object I32 extends ValType
  case object I64 extends ValType
  case object F32 extends ValType
  case object F64 extends ValType
}

case class ResultType(t: Option[ValType]) extends Type

case class FuncType(params: Vector[ValType], t: Vector[ValType]) extends Type

case class Limits(min: Int, max: Option[Int])

object Limits extends ((Int, Int) => Limits) {
  def apply(min: Int, max: Int): Limits =
    Limits(min, Some(max))
}

case class MemType(limits: Limits) extends Type

case class TableType(elemtype: ElemType, limits: Limits) extends Type

sealed trait ElemType extends Type

object ElemType {
  case object AnyFunc extends ElemType
}

case class GlobalType(valtype: ValType, mut: Mut) extends Type

sealed trait Mut

object Mut {
  case object Const extends Mut
  case object Var extends Mut
}

sealed trait ExternType extends Type

object ExternType {

  case class Func(functype: FuncType) extends ExternType
  case class Table(tabletype: TableType) extends ExternType
  case class Mem(memtype: MemType) extends ExternType
  case class Global(globaltype: GlobalType) extends ExternType

}
