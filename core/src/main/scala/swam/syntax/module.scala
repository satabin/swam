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

import scodec.bits.ByteVector

case class Module(
    types: Vector[FuncType],
    funcs: Vector[Func],
    tables: Vector[TableType],
    mems: Vector[MemType],
    globals: Vector[Global],
    elem: Vector[Elem],
    data: Vector[Data],
    start: Option[FuncIdx],
    imports: Vector[Import],
    exports: Vector[Export])

case class Func(
    tpe: TypeIdx,
    locals: Vector[ValType],
    body: Expr)

case class Global(tpe: GlobalType, init: Expr)

case class Elem(table: TableIdx, offset: Expr, init: Vector[FuncIdx])

case class Data(data: MemIdx, offset: Expr, init: ByteVector)

case class Import(module: Name, name: Name, desc: ImportDesc)

sealed trait ImportDesc

object ImportDesc {
  case class Func(idx: TypeIdx) extends ImportDesc
  case class Table(idx: TableIdx) extends ImportDesc
  case class Mem(idx: MemIdx) extends ImportDesc
  case class Global(idx: GlobalIdx) extends ImportDesc
}

case class Export(module: Name, name: Name, desc: ExportDesc)

sealed trait ExportDesc

object ExportDesc {
  case class Func(idx: TypeIdx) extends ExportDesc
  case class Table(idx: TableIdx) extends ExportDesc
  case class Mem(idx: MemIdx) extends ExportDesc
  case class Global(idx: GlobalIdx) extends ExportDesc
}
