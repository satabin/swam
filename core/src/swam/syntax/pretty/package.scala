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

import util.pretty._

package object pretty {

  implicit object LimitsPretty extends Pretty[Limits] {
    def pretty(l: Limits): Doc =
      l match {
        case Limits(min, Some(max)) => str("{min ") ++ int(min) ++ str(", max ") ++ int(max) ++ str("}")
        case Limits(min, None)      => str("{min ") ++ int(min) ++ str("}")
      }
  }

  implicit object MutPretty extends Pretty[Mut] {
    def pretty(m: Mut): Doc =
      m match {
        case Mut.Const => str("const")
        case Mut.Var   => str("var")
      }
  }

  implicit object TypePretty extends Pretty[Type] {
    def pretty(tpe: Type): Doc =
      tpe match {
        case ValType.I32      => str("i32")
        case ValType.I64      => str("i64")
        case ValType.F32      => str("f32")
        case ValType.F64      => str("f64")
        case ResultType(tpes) => str("[") ++ seq(str(", "), tpes) ++ str("]")
        case FuncType(params, result) =>
          str("[") ++ seq(str(", "), params) ++ str("] → [") ++ seq(str(", "), result) ++ str("]")
        case TableType(tpe, limits) => limits.pretty ++ space ++ tpe.pretty
        case MemType(limits)        => limits.pretty
        case GlobalType(tpe, mut)   => mut.pretty ++ space ++ tpe.pretty
        case ElemType.FuncRef       => str("funcref")
      }
  }

  implicit object BlockTypePretty extends Pretty[BlockType] {
    def pretty(bt: BlockType): Doc =
      bt match {
        case BlockType.NoType            => empty
        case BlockType.ValueType(tpe)    => tpe.pretty
        case BlockType.FunctionType(idx) => str("@") ++ int(idx)
      }
  }

  implicit object InstPretty extends Pretty[Inst] {

    def pretty(i: Inst): Doc =
      i match {
        case i32.Const(v)               => str(s"i32.const $v")
        case i32.Clz                    => str("i32.clz")
        case i32.Ctz                    => str("i32.ctz")
        case i32.Popcnt                 => str("i32.popcnt")
        case i32.Add                    => str("i32.add")
        case i32.Sub                    => str("i32.sub")
        case i32.Mul                    => str("i32.mul")
        case i32.DivS                   => str("i32.div_s")
        case i32.DivU                   => str("i32.div_u")
        case i32.RemS                   => str("i32.rem_s")
        case i32.RemU                   => str("i32.rem_u")
        case i32.And                    => str("i32.and")
        case i32.Or                     => str("i32.or")
        case i32.Xor                    => str("i32.xor")
        case i32.Shl                    => str("i32.shl")
        case i32.ShrS                   => str("i32.shr_s")
        case i32.ShrU                   => str("i32.shr_u")
        case i32.Rotl                   => str("i32.rotl")
        case i32.Rotr                   => str("i32.rotr")
        case i32.Eqz                    => str("i32.eqz")
        case i32.Eq                     => str("i32.eq")
        case i32.Ne                     => str("i32.ne")
        case i32.LtS                    => str("i32.lt_s")
        case i32.LtU                    => str("i32.lt_u")
        case i32.GtS                    => str("i32.gt_s")
        case i32.GtU                    => str("i32.gt_u")
        case i32.LeS                    => str("i32.le_s")
        case i32.LeU                    => str("i32.le_u")
        case i32.GeS                    => str("i32.ge_s")
        case i32.GeU                    => str("i32.ge_u")
        case i32.WrapI64                => str("i32.wrap_i64")
        case i32.TruncSF32              => str("i32.trunc_f32_s")
        case i32.TruncUF32              => str("i32.trunc_f32_u")
        case i32.TruncSF64              => str("i32.trunc_f64_s")
        case i32.TruncUF64              => str("i32.trunc_f64_u")
        case i32.TruncSatSF32           => str("i32.trunc_sat_f32_s")
        case i32.TruncSatUF32           => str("i32.trunc_sat_f32_u")
        case i32.TruncSatSF64           => str("i32.trunc_sat_f64_s")
        case i32.TruncSatUF64           => str("i32.trunc_sat_f64_u")
        case i32.ReinterpretF32         => str("i32.reinterpret_f32")
        case i32.Extend8S               => str("i32.extend8_s")
        case i32.Extend16S              => str("i32.extend16_s")
        case i32.Load(offset, align)    => str(s"i32.load {offset $offset, align $align}")
        case i32.Store(offset, align)   => str(s"i32.store {offset $offset, align $align}")
        case i32.Load8S(offset, align)  => str(s"i32.load8_s {offset $offset, align $align}")
        case i32.Load8U(offset, align)  => str(s"i32.load8_u {offset $offset, align $align}")
        case i32.Load16S(offset, align) => str(s"i32.load16_s {offset $offset, align $align}")
        case i32.Load16U(offset, align) => str(s"i32.load16_u {offset $offset, align $align}")
        case i32.Store8(offset, align)  => str(s"i32.store8 {offset $offset, align $align}")
        case i32.Store16(offset, align) => str(s"i32.store16 {offset $offset, align $align}")

        case i64.Const(v)               => str(s"i64.const $v")
        case i64.Clz                    => str("i64.clz")
        case i64.Ctz                    => str("i64.ctz")
        case i64.Popcnt                 => str("i64.popcnt")
        case i64.Add                    => str("i64.add")
        case i64.Sub                    => str("i64.sub")
        case i64.Mul                    => str("i64.mul")
        case i64.DivS                   => str("i64.div_s")
        case i64.DivU                   => str("i64.div_u")
        case i64.RemS                   => str("i64.rem_s")
        case i64.RemU                   => str("i64.rem_u")
        case i64.And                    => str("i64.and")
        case i64.Or                     => str("i64.or")
        case i64.Xor                    => str("i64.xor")
        case i64.Shl                    => str("i64.shl")
        case i64.ShrS                   => str("i64.shr_s")
        case i64.ShrU                   => str("i64.shr_u")
        case i64.Rotl                   => str("i64.rotl")
        case i64.Rotr                   => str("i64.rotr")
        case i64.Eqz                    => str("i64.eqz")
        case i64.Eq                     => str("i64.eq")
        case i64.Ne                     => str("i64.ne")
        case i64.LtS                    => str("i64.lt_s")
        case i64.LtU                    => str("i64.lt_u")
        case i64.GtS                    => str("i64.gt_s")
        case i64.GtU                    => str("i64.gt_u")
        case i64.LeS                    => str("i64.le_s")
        case i64.LeU                    => str("i64.le_u")
        case i64.GeS                    => str("i64.ge_s")
        case i64.GeU                    => str("i64.ge_u")
        case i64.ExtendSI32             => str("i64.extends_i32")
        case i64.ExtendUI32             => str("i64.extendu_i32")
        case i64.TruncSF32              => str("i64.trunc_f32_s")
        case i64.TruncUF32              => str("i64.trunc_f32_u")
        case i64.TruncSF64              => str("i64.trunc_f64_s")
        case i64.TruncUF64              => str("i64.trunc_f64_u")
        case i64.TruncSatSF32           => str("i64.trunc_sat_f32_s")
        case i64.TruncSatUF32           => str("i64.trunc_sat_f32_u")
        case i64.TruncSatSF64           => str("i64.trunc_sat_f64_s")
        case i64.TruncSatUF64           => str("i64.trunc_sat_f64_u")
        case i64.ReinterpretF64         => str("i64.reinterpret_f64")
        case i64.Extend8S               => str("i64.extend8_s")
        case i64.Extend16S              => str("i64.extend16_s")
        case i64.Extend32S              => str("i64.extend32_s")
        case i64.Load(offset, align)    => str(s"i64.load {offset $offset, align $align}")
        case i64.Store(offset, align)   => str(s"i64.store {offset $offset, align $align}")
        case i64.Load8S(offset, align)  => str(s"i64.load8_s {offset $offset, align $align}")
        case i64.Load8U(offset, align)  => str(s"i64.load8_u {offset $offset, align $align}")
        case i64.Load16S(offset, align) => str(s"i64.load16_s {offset $offset, align $align}")
        case i64.Load16U(offset, align) => str(s"i64.load16_u {offset $offset, align $align}")
        case i64.Load32S(offset, align) => str(s"i64.load32_s {offset $offset, align $align}")
        case i64.Load32U(offset, align) => str(s"i64.load32_u {offset $offset, align $align}")
        case i64.Store8(offset, align)  => str(s"i64.store8 {offset $offset, align $align}")
        case i64.Store16(offset, align) => str(s"i64.store16 {offset $offset, align $align}")
        case i64.Store32(offset, align) => str(s"i64.store32 {offset $offset, align $align}")

        case f32.Const(v)             => str(s"f32.const $v")
        case f32.Abs                  => str("f32.abs")
        case f32.Neg                  => str("f32.neg")
        case f32.Sqrt                 => str("f32.sqrt")
        case f32.Ceil                 => str("f32.ceil")
        case f32.Floor                => str("f32.floor")
        case f32.Trunc                => str("f32.trunc")
        case f32.Nearest              => str("f32.nearest")
        case f32.Add                  => str("f32.add")
        case f32.Sub                  => str("f32.sub")
        case f32.Mul                  => str("f32.mul")
        case f32.Div                  => str("f32.div")
        case f32.Min                  => str("f32.min")
        case f32.Max                  => str("f32.max")
        case f32.Copysign             => str("f32.copysign")
        case f32.Eq                   => str("f32.eq")
        case f32.Ne                   => str("f32.ne")
        case f32.Lt                   => str("f32.lt")
        case f32.Gt                   => str("f32.gt")
        case f32.Le                   => str("f32.le")
        case f32.Ge                   => str("f32.ge")
        case f32.DemoteF64            => str("f32.demote_f64")
        case f32.ConvertSI32          => str("f32.convert_i32_s")
        case f32.ConvertUI32          => str("f32.convert_i32_u")
        case f32.ConvertSI64          => str("f32.convert_i64_s")
        case f32.ConvertUI64          => str("f32.convert_i64_u")
        case f32.ReinterpretI32       => str("f32.reinterpret_i32")
        case f32.Load(offset, align)  => str(s"f32.load {offset $offset, align $align}")
        case f32.Store(offset, align) => str(s"f32.store {offset $offset, align $align}")

        case f64.Const(v)             => str(s"f64.const $v")
        case f64.Abs                  => str("f64.abs")
        case f64.Neg                  => str("f64.neg")
        case f64.Sqrt                 => str("f64.sqrt")
        case f64.Ceil                 => str("f64.ceil")
        case f64.Floor                => str("f64.floor")
        case f64.Trunc                => str("f64.trunc")
        case f64.Nearest              => str("f64.nearest")
        case f64.Add                  => str("f64.add")
        case f64.Sub                  => str("f64.sub")
        case f64.Mul                  => str("f64.mul")
        case f64.Div                  => str("f64.div")
        case f64.Min                  => str("f64.min")
        case f64.Max                  => str("f64.max")
        case f64.Copysign             => str("f64.copysign")
        case f64.Eq                   => str("f64.eq")
        case f64.Ne                   => str("f64.ne")
        case f64.Lt                   => str("f64.lt")
        case f64.Gt                   => str("f64.gt")
        case f64.Le                   => str("f64.le")
        case f64.Ge                   => str("f64.ge")
        case f64.PromoteF32           => str("f64.promote_f32")
        case f64.ConvertSI32          => str("f64.convert_i32_s")
        case f64.ConvertUI32          => str("f64.convert_i32_u")
        case f64.ConvertSI64          => str("f64.convert_i64_s")
        case f64.ConvertUI64          => str("f64.convert_i64_u")
        case f64.ReinterpretI64       => str("f64.reinterpret_i64")
        case f64.Load(offset, align)  => str(s"f64.load {offset $offset, align $align}")
        case f64.Store(offset, align) => str(s"f64.store {offset $offset, align $align}")

        case Drop   => str("drop")
        case Select => str("select")

        case LocalGet(idx)  => str(s"local.get $idx")
        case LocalSet(idx)  => str(s"local.set $idx")
        case LocalTee(idx)  => str(s"local.tee $idx")
        case GlobalGet(idx) => str(s"global.get $idx")
        case GlobalSet(idx) => str(s"global.set $idx")

        case MemorySize => str("memory.size")
        case MemoryGrow => str("memory.grow")

        case Nop         => str("nop")
        case Unreachable => str("unreachable")

        case Block(tpe, is) =>
          group(str("block") ++ space ++ tpe.pretty ++ nest(2, group(line ++ seq(line, is)) ++ line ++ str("end")))

        case Loop(tpe, is) =>
          group(str("loop") ++ space ++ tpe.pretty ++ nest(2, group(line ++ seq(line, is)) ++ line ++ str("end")))

        case If(tpe, t, e) =>
          group(
            str("if") ++ space ++ tpe.pretty ++ nest(2, group(line ++ seq(line, t))) ++ line ++ str("else") ++ nest(
              2,
              group(line ++ seq(line, e))) ++ line ++ str("end"))

        case Br(lbl)             => str(s"br $lbl")
        case BrIf(lbl)           => str(s"br_if $lbl")
        case BrTable(table, lbl) => str(s"br_table ${table.mkString(", ")} $lbl")
        case Return              => str("return")
        case Call(idx)           => str(s"call $idx")
        case CallIndirect(tpe)   => str(s"call_indirect $tpe")
      }

  }

  implicit object LocalEntryPretty extends Pretty[LocalEntry] {
    def pretty(le: LocalEntry): Doc =
      seq(newline, for (_ <- 0 until le.count) yield str("local") ++ space ++ le.tpe.pretty)
  }

  implicit object ImportPretty extends Pretty[Import] {
    def pretty(i: Import): Doc =
      i match {
        case Import.Function(module, field, tpe) =>
          str("{module ") ++ str(module) ++ str(", name ") ++ str(field) ++ str(", func ") ++ int(tpe) ++ str("}")
        case Import.Table(module, field, tpe) =>
          str("{module ") ++ str(module) ++ str(", name ") ++ str(field) ++ str(", table ") ++ tpe.pretty ++ str("}")
        case Import.Memory(module, field, tpe) =>
          str("{module ") ++ str(module) ++ str(", name ") ++ str(field) ++ str(",  mem ") ++ tpe.pretty ++ str("}")
        case Import.Global(module, field, tpe) =>
          str("{module ") ++ str(module) ++ str(", name ") ++ str(field) ++ str(",  global ") ++ tpe.pretty ++ str("}")
      }
  }

  implicit object ExportPretty extends Pretty[Export] {
    def pretty(e: Export): Doc = {
      val kind = e.kind match {
        case ExternalKind.Function => str("func ")
        case ExternalKind.Table    => str("table ")
        case ExternalKind.Memory   => str("mem ")
        case ExternalKind.Global   => str("global ")
      }
      str("{name ") ++ str(e.fieldName) ++ str(", ") ++ kind ++ int(e.index) ++ str("}")
    }
  }

  implicit object GlobalPretty extends Pretty[Global] {
    def pretty(g: Global): Doc =
      group(g.tpe.pretty ++ line ++ seq(line, g.init))
  }

  implicit object FuncBodyPretty extends Pretty[FuncBody] {
    def pretty(body: FuncBody): Doc =
      if (body.locals.isEmpty)
        seq(newline, body.code)
      else
        seq(newline, body.locals) ++ newline ++ seq(newline, body.code)
  }

  implicit object ElemPretty extends Pretty[Elem] {
    def pretty(elem: Elem): Doc =
      str("; table") ++ newline ++ int(elem.table) ++ newline ++ str("; offset") ++ newline ++ seq(newline, elem.offset) ++ newline ++ indexedseq(
        str("; func "),
        elem.init)
  }

  implicit object DataPretty extends Pretty[Data] {
    def pretty(data: Data): Doc =
      str("; memory") ++ newline ++ int(data.data) ++ newline ++ str("; offset") ++ newline ++ seq(newline, data.offset) ++ newline ++ str(
        "; data") ++ str(data.init.toHex)
  }

  implicit object SectionPretty extends Pretty[Section] {
    def pretty(s: Section): Doc =
      s match {
        case Section.Types(types) =>
          str("""; section "Types"""") ++ newline ++ indexedseq(str("; type "), types)
        case Section.Imports(imports) =>
          str("""; section "Imports"""") ++ newline ++ group(seq(newline, imports))
        case Section.Functions(tpes) =>
          str("""; section "Functions"""") ++ newline ++ indexedseq(str("; function type "), tpes)
        case Section.Tables(tables) =>
          str("""; section "Tables"""") ++ newline ++ indexedseq(str("; table "), tables)
        case Section.Memories(mems) =>
          str("""; section "Memories"""") ++ newline ++ indexedseq(str("; memory "), mems)
        case Section.Globals(globals) =>
          str("""; section "Globals"""") ++ newline ++ indexedseq(str("; global "), globals)
        case Section.Exports(exports) =>
          str("""; section "Exports"""") ++ newline ++ group(seq(newline, exports))
        case Section.Start(idx) =>
          str("""; section "Start"""") ++ newline ++ str("func ") ++ int(idx)
        case Section.Elements(elts) =>
          str("""; section "Elements"""") ++ newline ++ seq(newline, elts)
        case Section.Code(bodies) =>
          str("""; section "Code"""") ++ newline ++ group(indexedseq(str("; function body "), bodies))
        case Section.Datas(data) =>
          str("""; section "Data"""") ++ newline ++ seq(newline, data)
        case Section.Custom(name, payload) =>
          str("""; custom section """") ++ str(name) ++ str("\"") ++ newline ++ str(payload.toHex)
      }
  }

  private def indexedseq[T](prefix: Doc, s: Seq[T])(implicit T: Pretty[T]): Doc =
    group(seq(newline, s.zipWithIndex.map {
      case (t, idx) => prefix ++ int(idx) ++ newline ++ t.pretty
    }))

}
