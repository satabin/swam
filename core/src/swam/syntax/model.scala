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
package syntax

import scodec.bits.BitVector

case class Global(tpe: GlobalType, init: Expr)

case class Elem(table: TableIdx, offset: Expr, init: Vector[FuncIdx])

case class Data(data: MemIdx, offset: Expr, init: BitVector)

sealed trait ExternalKind
object ExternalKind {
  case object Function extends ExternalKind
  case object Table extends ExternalKind
  case object Memory extends ExternalKind
  case object Global extends ExternalKind
}

sealed abstract class Section(val id: Int)
object Section {
  case class Types(types: Vector[FuncType]) extends Section(1)
  case class Imports(imports: Vector[Import]) extends Section(2)
  case class Functions(functions: Vector[Int]) extends Section(3)
  case class Tables(tables: Vector[TableType]) extends Section(4)
  case class Memories(memories: Vector[MemType]) extends Section(5)
  case class Globals(globals: Vector[Global]) extends Section(6)
  case class Exports(exports: Vector[Export]) extends Section(7)
  case class Start(index: FuncIdx) extends Section(8)
  case class Elements(elements: Vector[Elem]) extends Section(9)
  case class Code(bodies: Vector[FuncBody]) extends Section(10)
  case class Datas(data: Vector[Data]) extends Section(11)
  case class Custom(name: String, payload: BitVector) extends Section(0)
}

sealed trait Import {
  val moduleName: String
  val fieldName: String
}
object Import {

  def unapply(e: Import): Option[(String, String)] =
    Some(e.moduleName -> e.fieldName)

  case class Function(moduleName: String, fieldName: String, tpe: Int) extends Import
  case class Table(moduleName: String, fieldName: String, tpe: TableType) extends Import
  case class Memory(moduleName: String, fieldName: String, tpe: MemType) extends Import
  case class Global(moduleName: String, fieldName: String, tpe: GlobalType) extends Import
}

case class Export(fieldName: String, kind: ExternalKind, index: Int)

case class FuncBody(locals: Vector[LocalEntry], code: Expr)

case class LocalEntry(count: Int, tpe: ValType)
