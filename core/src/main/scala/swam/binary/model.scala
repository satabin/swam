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
package binary

import scodec.bits.ByteVector

case class Global(tpe: GlobalType, init: Expr)

case class Elem(table: TableIdx, offset: Expr, init: Vector[FuncIdx])

case class Data(data: MemIdx, offset: Expr, init: ByteVector)

sealed trait ExternalKind
object ExternalKind {
  case object Function extends ExternalKind
  case object Table extends ExternalKind
  case object Memory extends ExternalKind
  case object Global extends ExternalKind
}

sealed trait Section
object Section {
  case class Types(types: Vector[FuncType]) extends Section
  case class Imports(imports: Vector[ImportEntry]) extends Section
  case class Functions(functions: Vector[Int]) extends Section
  case class Tables(tables: Vector[TableType]) extends Section
  case class Memories(memories: Vector[MemType]) extends Section
  case class Globals(globals: Vector[Global]) extends Section
  case class Exports(exports: Vector[ExportEntry]) extends Section
  case class Start(index: FuncIdx) extends Section
  case class Elements(elements: Vector[Elem]) extends Section
  case class Code(bodies: Vector[FuncBody]) extends Section
  case class Datas(data: Vector[Data]) extends Section
  case class Custom(name: String, payload: ByteVector) extends Section
}

sealed trait ImportEntry {
  val moduleName: String
  val fieldName: String
}
object ImportEntry {

  def unapply(e: ImportEntry): Option[(String, String)] =
    Some(e.moduleName -> e.fieldName)

  case class Function(moduleName: String, fieldName: String, tpe: Int) extends ImportEntry
  case class Table(moduleName: String, fieldName: String, tpe: TableType) extends ImportEntry
  case class Memory(moduleName: String, fieldName: String, tpe: MemType) extends ImportEntry
  case class Global(moduleName: String, fieldName: String, tpe: GlobalType) extends ImportEntry
}

case class ExportEntry(fieldName: String, kind: ExternalKind, index: Int)

case class FuncBody(locals: Vector[LocalEntry], code: ByteVector)

case class LocalEntry(count: Int, tpe: ValType)
