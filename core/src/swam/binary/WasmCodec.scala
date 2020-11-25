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

object WasmCodec extends InstCodec {

  val externalKind: Codec[ExternalKind] =
    mappedEnum(byte,
               Map[ExternalKind, Byte](ExternalKind.Function -> 0,
                                       ExternalKind.Table -> 1,
                                       ExternalKind.Memory -> 2,
                                       ExternalKind.Global -> 3))

  val types: Codec[Vector[FuncType]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, funcType))

  val importEntry: Codec[String ~ String ~ Import] =
    (("module" | variableSizeBytes(varuint32, utf8)) ~
      ("field" | variableSizeBytes(varuint32, utf8))).flatZip {
      case (module, field) =>
        discriminated[Import]
          .by(externalKind)
          .|(ExternalKind.Function) { case Import.Function(_, _, tpe) => tpe }(Import.Function(module, field, _))(
            varuint32
          )
          .|(ExternalKind.Table) { case Import.Table(_, _, tpe) => tpe }(Import.Table(module, field, _))(tableType)
          .|(ExternalKind.Memory) { case Import.Memory(_, _, tpe) => tpe }(Import.Memory(module, field, _))(memoryType)
          .|(ExternalKind.Global) { case Import.Global(_, _, tpe) => tpe }(Import.Global(module, field, _))(globalType)
    }

  val imports: Codec[Vector[Import]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, importEntry.xmap({
      case (_, e)                  => e
    }, { case e @ Import(mod, fld) => ((mod, fld), e) })))

  val functions: Codec[Vector[Int]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, varuint32))

  val tables: Codec[Vector[TableType]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, tableType))

  val mems: Codec[Vector[MemType]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, memoryType))

  val globalVariable: Codec[Global] =
    (globalType :: expr).as[Global]

  val globals: Codec[Vector[Global]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, globalVariable))

  val elemkind: Codec[ElemType] =
    constant(hex"00") ~> provide(ElemType.FuncRef)

  val elemexpr: Codec[ElemExpr] =
    discriminated
      .by(byte)
      .typecase((0xd0 & 0xff).toByte, constant(hex"700b") ~> provide(ElemExpr.RefNull).upcast[ElemExpr])
      .typecase(
        (0xd2 & 0xff).toByte,
        varuint32.widen[ElemExpr](ElemExpr.RefFunc(_), {
          case ElemExpr.RefFunc(idx) => Attempt.successful(idx)
          case ElemExpr.RefNull      => Attempt.failure(Err("ref.func expected"))
        }) <~ constant(hex"0b")
      )

  val elemSegment: Codec[Elem] =
    discriminated[Elem]
      .by(byte)
      .|(0x00) {
        case Elem(ElemType.FuncRef, init, ElemMode.Active(0, e)) if init.forall(_.isFuncRef) =>
          (e, init.collect { case ElemExpr.RefFunc(idx) => idx })
      } {
        case (e, init) => Elem(ElemType.FuncRef, init.map(ElemExpr.RefFunc(_)), ElemMode.Active(0, e))
      }(expr ~ vectorOfN(varuint32, varuint32))
      .|(0x01) {
        case Elem(kind, init, ElemMode.Passive) if init.forall(_.isFuncRef) =>
          (kind, init.collect { case ElemExpr.RefFunc(idx) => idx })
      } { case (kind, init) => Elem(kind, init.map(ElemExpr.RefFunc(_)), ElemMode.Passive) }(
        elemkind ~ vectorOfN(varuint32, varuint32))
      .|(0x02) {
        case Elem(kind, init, ElemMode.Active(idx, e)) if init.forall(_.isFuncRef) =>
          idx ~ e ~ kind ~ init.collect { case ElemExpr.RefFunc(idx) => idx }
      } {
        case idx ~ e ~ kind ~ init => Elem(kind, init.map(ElemExpr.RefFunc(_)), ElemMode.Active(idx, e))
      }(varuint32 ~ expr ~ elemkind ~ vectorOfN(varuint32, varuint32))
      .|(0x04) {
        case Elem(ElemType.FuncRef, init, ElemMode.Active(0, e)) =>
          (e, init)
      } {
        case (e, init) => Elem(ElemType.FuncRef, init, ElemMode.Active(0, e))
      }(expr ~ vectorOfN(varuint32, elemexpr))
      .|(0x05) {
        case Elem(kind, init, ElemMode.Passive) =>
          (kind, init)
      } { case (kind, init) => Elem(kind, init, ElemMode.Passive) }(elemkind ~ vectorOfN(varuint32, elemexpr))
      .|(0x06) {
        case Elem(kind, init, ElemMode.Active(idx, e)) =>
          idx ~ e ~ kind ~ init
      } {
        case idx ~ e ~ kind ~ init => Elem(kind, init, ElemMode.Active(idx, e))
      }(varuint32 ~ expr ~ elemkind ~ vectorOfN(varuint32, elemexpr))

  val elem: Codec[Vector[Elem]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, elemSegment))

  val dataSegment: Codec[Data] =
    discriminated[Data]
      .by(byte)
      .|(0x00) { case Data(b, DataMode.Active(0, e)) => (e, b) } { case (e, b) => Data(b, DataMode.Active(0, e)) }(
        expr ~ variableSizeBytes(varuint32, scodec.codecs.bits))
      .|(0x01) { case Data(b, DataMode.Passive) => b }(Data(_, DataMode.Passive))(variableSizeBytes(varuint32,
                                                                                                    scodec.codecs.bits))
      .|(0x02) { case Data(b, DataMode.Active(idx, e)) => idx ~ e ~ b } {
        case idx ~ e ~ b => Data(b, DataMode.Active(idx, e))
      }(varuint32 ~ expr ~ variableSizeBytes(varuint32, scodec.codecs.bits))

  val data: Codec[Vector[Data]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, dataSegment))

  val dataCount: Codec[Int] =
    variableSizeBytes(varuint32, varuint32)

  val start: Codec[FuncIdx] =
    variableSizeBytes(varuint32, varuint32)

  val localEntry: Codec[LocalEntry] =
    (varuint32 :: valType).as[LocalEntry]

  val funcBody: Codec[FuncBody] =
    variableSizeBytes(varuint32,
                      (("locals" | vectorOfN(varuint32, localEntry)) ::
                        ("code" | expr)).as[FuncBody])

  val code: Codec[Vector[FuncBody]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, funcBody))

  val exportEntry: Codec[Export] =
    (("field" | variableSizeBytes(varuint32, utf8)) ::
      ("kind" | externalKind) ::
      ("index" | varuint32)).as[Export]

  val exports: Codec[Vector[Export]] =
    variableSizeBytes(varuint32, vectorWithN(varuint32, exportEntry))

  val custom: Codec[(String, BitVector)] =
    variableSizeBytes(varuint32,
                      ("name" | variableSizeBytes(varuint32, utf8)) ~
                        ("payload" | scodec.codecs.bits))

  val section =
    discriminated[Section]
      .by(varuint7)
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
      .|(12) { case Section.DataCount(count) => count }(Section.DataCount)(dataCount)

}
