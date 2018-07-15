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
package text
package unresolved

sealed trait Field {
  val pos: Int
}

case class Type(id: Id, pnames: Vector[Id], tpe: FuncType)(val pos: Int) extends Field

case class Import(mod: String, name: String, desc: ImportDesc)(val pos: Int) extends Field

sealed trait ImportDesc
object ImportDesc {
  case class Func(id: Id, typeuse: TypeUse)(val pos: Int) extends ImportDesc
  case class Table(id: Id, tpe: TableType)(val pos: Int) extends ImportDesc
  case class Memory(id: Id, tpe: MemType)(val pos: Int) extends ImportDesc
  case class Global(id: Id, tpe: GlobalType)(val pos: Int) extends ImportDesc
}

case class Function(id: Id, typeuse: TypeUse, locals: Seq[Local], is: Seq[Inst])(val pos: Int) extends Field

case class Local(id: Id, tpe: ValType)(val pos: Int)

case class Table(id: Id, tpe: TableType)(val pos: Int) extends Field

case class Memory(id: Id, tpe: MemType)(val pos: Int) extends Field

case class Global(id: Id, tpe: GlobalType, init: Expr)(val pos: Int) extends Field

case class Export(name: String, desc: ExportDesc)(val pos: Int) extends Field

sealed trait ExportDesc
object ExportDesc {
  case class Func(index: Index)(val pos: Int) extends ExportDesc
  case class Table(index: Index)(val pos: Int) extends ExportDesc
  case class Memory(index: Index)(val pos: Int) extends ExportDesc
  case class Global(index: Index)(val pos: Int) extends ExportDesc
}

case class StartFunc(index: Index)(val pos: Int) extends Field

case class Elem(table: Index, offset: Expr, init: Seq[Index])(val pos: Int) extends Field

case class Data(mem: Index, offset: Expr, data: String)(val pos: Int) extends Field

case class Module(id: Id, fields: Seq[Field])(val pos: Int)
