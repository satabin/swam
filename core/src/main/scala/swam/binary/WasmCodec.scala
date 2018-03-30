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

import syntax._

import scodec._
import scodec.Codec
import scodec.bits._
import scodec.codecs._
import scodec.codecs.literals._

import scala.annotation.tailrec

object WasmCodec {

  private val varuint1: Codec[Int] = new Varuint(1)

  private val varuint7: Codec[Int] = new Varuint(7)

  private val varint7: Codec[Int] = new Varint(7)

  private val varuint32: Codec[Int] = new Varuint(31)

  private val varint32: Codec[Int] = new Varint(32)

  private val varint64: Codec[Long] = new Varlong(64)

  private val valType: Codec[ValType] =
    mappedEnum(
      byte,
      Map[ValType, Byte](
        ValType.I32 -> 0x7f,
        ValType.I64 -> 0x7e,
        ValType.F32 -> 0x7d,
        ValType.F64 -> 0x7c))

  private val blockType: Codec[Option[ValType]] =
    mappedEnum(
      byte,
      Map[Option[ValType], Byte](
        Some(ValType.I32) -> 0x7f,
        Some(ValType.I64) -> 0x7e,
        Some(ValType.F32) -> 0x7d,
        Some(ValType.F64) -> 0x7c,
        None -> 0x40))

  private val elemType: Codec[ElemType] =
    mappedEnum(
      byte,
      Map[ElemType, Byte](
        ElemType.AnyFunc -> 0x70))

  private val funcType: Codec[FuncType] =
    mappedEnum(
      byte,
      Map[Unit, Byte](
        () -> 0x60)) ~>
      (("parameters" | vectorOfN(varuint32, valType)) ::
        ("return" | vectorOfN(varuint1, valType))).as[FuncType]

  private val globalType: Codec[GlobalType] =
    (("content_type" | valType) ::
      ("mutability" | mappedEnum(
        byte,
        Map[Mut, Byte](
          Mut.Const -> 0x00,
          Mut.Var -> 0x01)))).as[GlobalType]

  private val limits: Codec[Limits] =
    discriminated[Limits].by(varuint1)
      .|(0) { case Limits(min, None) => min }(Limits(_, None))(varuint32)
      .|(1) { case Limits(min, Some(max)) => (min, max) }(Limits.tupled)(varuint32 ~ varuint32)

  private val tableType: Codec[TableType] =
    (("element_type" | elemType) ::
      ("limits" | limits)).as[TableType]

  private val memoryType: Codec[MemType] =
    ("limits" | limits).as[MemType]

  private val externalKind: Codec[ExternalKind] =
    mappedEnum(byte, Map[ExternalKind, Byte](
      ExternalKind.Function -> 0,
      ExternalKind.Table -> 1,
      ExternalKind.Memory -> 2,
      ExternalKind.Global -> 3))

  private val types: Codec[Vector[FuncType]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, funcType))

  private val importEntry: Codec[String ~ String ~ ImportEntry] =
    (("module" | variableSizeBytes(varuint32, utf8)) ~
      ("field" | variableSizeBytes(varuint32, utf8))).flatZip {
        case (module, field) =>
          discriminated[ImportEntry].by(externalKind)
            .|(ExternalKind.Function) { case ImportEntry.Function(_, _, tpe) => tpe }(ImportEntry.Function(module, field, _))(varuint32)
            .|(ExternalKind.Table) { case ImportEntry.Table(_, _, tpe) => tpe }(ImportEntry.Table(module, field, _))(tableType)
            .|(ExternalKind.Memory) { case ImportEntry.Memory(_, _, tpe) => tpe }(ImportEntry.Memory(module, field, _))(memoryType)
            .|(ExternalKind.Global) { case ImportEntry.Global(_, _, tpe) => tpe }(ImportEntry.Global(module, field, _))(globalType)
      }

  private val imports: Codec[Vector[ImportEntry]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, importEntry.xmap({ case (_, e) => e }, { case e @ ImportEntry(mod, fld) => ((mod, fld), e) })))

  private val functions: Codec[Vector[Int]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, varuint32))

  private val tables: Codec[Vector[TableType]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, tableType))

  private val mems: Codec[Vector[MemType]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, memoryType))

  private val end: Codec[Unit] =
    constant(0x0b)

  private val expr: Codec[Expr] =
    ???

  private val globalVariable: Codec[Global] =
    (globalType :: expr).as[Global]

  private val globals: Codec[Vector[Global]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, globalVariable))

  private val elemSegment: Codec[Elem] =
    (("index" | varuint32) ::
      ("offset" | expr) ::
      ("elems" | vectorOfN(varuint32, varuint32))).as[Elem]

  private val elem: Codec[Vector[Elem]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, elemSegment))

  private val dataSegment: Codec[Data] =
    (("index" | varuint32) ::
      ("offset" | expr) ::
      ("data" | variableSizeBytes(varuint32, bytes))).as[Data]

  private val data: Codec[Vector[Data]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, dataSegment))

  private val start: Codec[FuncIdx] =
    variableSizeBytes(varuint32, varuint32)

  private val localEntry: Codec[LocalEntry] =
    (varuint32 :: valType).as[LocalEntry]

  private val funcBody: Codec[FuncBody] =
    variableSizeBytes(
      varuint32,
      (("locals" | vectorOfN(varuint32, localEntry)) ::
        ("code" | bytes)).as[FuncBody])

  private val code: Codec[Vector[FuncBody]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, funcBody))

  private val exportEntry: Codec[ExportEntry] =
    (("field" | variableSizeBytes(varuint32, utf8)) ::
      ("kind" | externalKind) ::
      ("index" | varuint32)).as[ExportEntry]

  private val exports: Codec[Vector[ExportEntry]] =
    variableSizeBytes(varuint32, vectorOfN(varuint32, exportEntry))

  private val custom: Codec[(String, ByteVector)] =
    variableSizeBytes(
      varuint32,
      (("name" | variableSizeBytes(varuint32, utf8)) ~
        ("payload" | bytes)))

  val section =
    discriminated[Section].by(varuint7)
      .|(0) { case Section.Custom(name, payload) => (name, payload) }(Section.Custom.tupled)(custom)
      .|(1) { case Section.Types(types) => types }(Section.Types)(types)
      .|(2) { case Section.Imports(imports) => imports }(Section.Imports)(imports)
      .|(3) { case Section.Functions(functions) => functions }(Section.Functions)(functions)
      .|(4) { case Section.Tables(tables) => tables }(Section.Tables)(tables)
      .|(5) { case Section.Memories(mems) => mems }(Section.Memories)(mems)
      .|(6) { case Section.Globals(globals) => globals }(Section.Globals)(globals)
      .|(7) { case Section.Exports(exports) => exports }(Section.Exports)(exports)
      .|(8) { case Section.Start(start) => start }(Section.Start)(start)
      .|(9) { case Section.Elements(elem) => elem }(Section.Elements)(elem)
      .|(10) { case Section.Code(code) => code }(Section.Code)(code)
      .|(11) { case Section.Datas(data) => data }(Section.Datas)(data)

}
