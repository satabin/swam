/*
 * Copyright 2018 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http:/_www.apache.org_licenses_LICENSE-2.0
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

import util.pretty._

import scala.annotation.tailrec

package object pretty {

  implicit object IndexPretty extends Pretty[Index] {
    def pretty(idx: Index): Doc =
      idx match {
        case Left(i)           => space ++ int(i)
        case Right(SomeId(n))  => space ++ str("$") ++ str(n)
        case Right(FreshId(_)) => empty
        case Right(NoId)       => empty
      }
  }

  implicit object IdPretty extends Pretty[Id] {
    def pretty(id: Id): Doc =
      id match {
        case SomeId(n)  => space ++ str("$") ++ str(n)
        case FreshId(_) => empty
        case NoId       => empty
      }
  }

  implicit object ValTypePretty extends Pretty[ValType] {
    def pretty(tpe: ValType): Doc =
      tpe match {
        case ValType.I32 => str("i32")
        case ValType.I64 => str("i64")
        case ValType.F32 => str("f32")
        case ValType.F64 => str("f64")
      }
  }

  implicit object ResultTypePretty extends Pretty[ResultType] {
    def pretty(tpe: ResultType): Doc =
      tpe match {
        case ResultType(Vector()) => empty
        case ResultType(ts)       => space ++ seq(space, ts.map(t => str("(result ") ++ t.pretty ++ str(")")))
      }
  }

  implicit object TypeUsePretty extends Pretty[TypeUse] {
    def pretty(tu: TypeUse): Doc =
      tu match {
        case TypeUse(None, params, results) => functype(params, results)
        case TypeUse(Some(tpe), params, results) =>
          line ++ group(nest(2, str("(type") ++ line ++ tpe.pretty ++ functype(params, results)))
      }
  }

  private def functype(ps: Vector[Param], rs: Vector[ValType]): Doc =
    params(ps) ++ results(rs)

  private def params(ps: Vector[Param]): Doc =
    if (ps.isEmpty)
      empty
    else
      line ++ seq(line, ps.map {
        case (SomeId(n), tpe) => str("(param ") ++ str("$") ++ str(n) ++ space ++ tpe.pretty ++ str(")")
        case (_, tpe)         => str("(param ") ++ tpe.pretty ++ str(")")
      })

  private def results(rs: Vector[ValType]): Doc =
    if (rs.isEmpty)
      empty
    else
      line ++ seq(line, rs.map(r => str("(result ") ++ r.pretty ++ str(")")))

  /** Simple expression rendering with not folding at all. */
  object simple {
    implicit object ExprPretty extends Pretty[Expr] {
      def pretty(e: Expr): Doc =
        seq(line, e)
    }
  }

  /** Untyped folded-like pretty printing. Unary and binary operators
    * are represented folded, as well as for if and for.
    * However, function calls are not and this may result in some weird-looking,
    * albeit correct, rendering.
    */
  object untyped {
    implicit object ExprPretty extends Pretty[Expr] {
      def pretty(e: Expr): Doc = {
        @tailrec
        def loop(e: Expr, acc: List[Doc]): Doc =
          (e, acc) match {
            case (Seq(), _) => seq(line, acc.reverse)
            case (Seq(op @ Binop(_), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op @ Unop(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ Testop(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ Relop(_), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op @ Convertop(_, _), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(i @ If(_, _, _, _, _, _), rest @ _*), d :: restd) =>
              loop(rest, foldedif(i, d) :: restd)
            case (Seq(op @ BrIf(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ LocalSet(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ LocalTee(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ GlobalSet(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ Store(_, _, _), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op @ StoreN(_, _, _, _), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op, rest @ _*), _) =>
              loop(rest, InstPretty.pretty(op) :: acc)
          }
        loop(e, Nil)
      }
    }
  }

  implicit def InstPretty(implicit E: Pretty[Expr]): Pretty[Inst] = new Pretty[Inst] {
    def pretty(i: Inst): Doc =
      i match {
        case i32.Const(v)               => str("i32.const ") ++ int(v)
        case i32.Clz()                  => str("i32.clz")
        case i32.Ctz()                  => str("i32.ctz")
        case i32.Popcnt()               => str("i32.popcnt")
        case i32.Extend8S()             => str("i32.extend8_s")
        case i32.Extend16S()            => str("i32.extend16_s")
        case i32.Add()                  => str("i32.add")
        case i32.Sub()                  => str("i32.sub")
        case i32.Mul()                  => str("i32.mul")
        case i32.DivS()                 => str("i32.div_s")
        case i32.DivU()                 => str("i32.div_u")
        case i32.RemS()                 => str("i32.rem_s")
        case i32.RemU()                 => str("i32.rem_u")
        case i32.And()                  => str("i32.and")
        case i32.Or()                   => str("i32.or")
        case i32.Xor()                  => str("i32.xor")
        case i32.Shl()                  => str("i32.shl")
        case i32.ShrS()                 => str("i32.shr_s")
        case i32.ShrU()                 => str("i32.shr_u")
        case i32.Rotl()                 => str("i32.rotl")
        case i32.Rotr()                 => str("i32.rotr")
        case i32.Eqz()                  => str("i32.eqz")
        case i32.Eq()                   => str("i32.eq")
        case i32.Ne()                   => str("i32.ne")
        case i32.LtS()                  => str("i32.lt_s")
        case i32.LtU()                  => str("i32.lt_u")
        case i32.GtS()                  => str("i32.gt_s")
        case i32.GtU()                  => str("i32.gt_u")
        case i32.LeS()                  => str("i32.le_s")
        case i32.LeU()                  => str("i32.le_u")
        case i32.GeS()                  => str("i32.ge_s")
        case i32.GeU()                  => str("i32.ge_u")
        case i32.WrapI64()              => str("i32.wrap_i64")
        case i32.TruncSF32()            => str("i32.trunc_f32_s")
        case i32.TruncUF32()            => str("i32.trunc_f32_u")
        case i32.TruncSF64()            => str("i32.trunc_f64_s")
        case i32.TruncUF64()            => str("i32.trunc_f64_u")
        case i32.TruncSatSF32()         => str("i32.trunc_sat_f32_s")
        case i32.TruncSatUF32()         => str("i32.trunc_sat_f32_u")
        case i32.TruncSatSF64()         => str("i32.trunc_sat_f64_s")
        case i32.TruncSatUF64()         => str("i32.trunc_sat_f64_u")
        case i32.ReinterpretF32()       => str("i32.reinterpret_f32")
        case i32.Load(align, offset)    => group(nest(2, str("i32.load") ++ memarg(align, offset, 2)))
        case i32.Store(align, offset)   => group(nest(2, str("i32.store") ++ memarg(align, offset, 2)))
        case i32.Load8S(align, offset)  => group(nest(2, str("i32.load8_s") ++ memarg(align, offset, 0)))
        case i32.Load8U(align, offset)  => group(nest(2, str("i32.load8_u") ++ memarg(align, offset, 0)))
        case i32.Load16S(align, offset) => group(nest(2, str("i32.load16_s") ++ memarg(align, offset, 1)))
        case i32.Load16U(align, offset) => group(nest(2, str("i32.load16_u") ++ memarg(align, offset, 1)))
        case i32.Store8(align, offset)  => group(nest(2, str("i32.store8") ++ memarg(align, offset, 0)))
        case i32.Store16(align, offset) => group(nest(2, str("i32.store16") ++ memarg(align, offset, 1)))

        case i64.Const(v)               => str("i64.const ") ++ long(v)
        case i64.Clz()                  => str("i64.clz")
        case i64.Ctz()                  => str("i64.ctz")
        case i64.Popcnt()               => str("i64.popcnt")
        case i64.Extend8S()             => str("i64.extend8_s")
        case i64.Extend16S()            => str("i64.extend16_s")
        case i64.Extend32S()            => str("i64.extend32_s")
        case i64.Add()                  => str("i64.add")
        case i64.Sub()                  => str("i64.sub")
        case i64.Mul()                  => str("i64.mul")
        case i64.DivS()                 => str("i64.div_s")
        case i64.DivU()                 => str("i64.div_u")
        case i64.RemS()                 => str("i64.rem_s")
        case i64.RemU()                 => str("i64.rem_u")
        case i64.And()                  => str("i64.and")
        case i64.Or()                   => str("i64.or")
        case i64.Xor()                  => str("i64.xor")
        case i64.Shl()                  => str("i64.shl")
        case i64.ShrS()                 => str("i64.shr_s")
        case i64.ShrU()                 => str("i64.shr_u")
        case i64.Rotl()                 => str("i64.rotl")
        case i64.Rotr()                 => str("i64.rotr")
        case i64.Eqz()                  => str("i64.eqz")
        case i64.Eq()                   => str("i64.eq")
        case i64.Ne()                   => str("i64.ne")
        case i64.LtS()                  => str("i64.lt_s")
        case i64.LtU()                  => str("i64.lt_u")
        case i64.GtS()                  => str("i64.gt_s")
        case i64.GtU()                  => str("i64.gt_u")
        case i64.LeS()                  => str("i64.le_s")
        case i64.LeU()                  => str("i64.le_u")
        case i64.GeS()                  => str("i64.ge_s")
        case i64.GeU()                  => str("i64.ge_u")
        case i64.ExtendSI32()           => str("i64.extend_i32_s")
        case i64.ExtendUI32()           => str("i64.extend_i32_u")
        case i64.TruncSF32()            => str("i64.trunc_f32_s")
        case i64.TruncUF32()            => str("i64.trunc_f32_u")
        case i64.TruncSF64()            => str("i64.trunc_f64_s")
        case i64.TruncUF64()            => str("i64.trunc_f64_u")
        case i64.TruncSatSF32()         => str("i64.trunc_sat_f32_s")
        case i64.TruncSatUF32()         => str("i64.trunc_sat_f32_u")
        case i64.TruncSatSF64()         => str("i64.trunc_sat_f64_s")
        case i64.TruncSatUF64()         => str("i64.trunc_sat_f64_u")
        case i64.ReinterpretF64()       => str("i64.reinterpret_f64")
        case i64.Load(align, offset)    => group(nest(2, str("i64.load") ++ memarg(align, offset, 4)))
        case i64.Store(align, offset)   => group(nest(2, str("i64.store") ++ memarg(align, offset, 4)))
        case i64.Load8S(align, offset)  => group(nest(2, str("i64.load8_s") ++ memarg(align, offset, 0)))
        case i64.Load8U(align, offset)  => group(nest(2, str("i64.load8_u") ++ memarg(align, offset, 0)))
        case i64.Load16S(align, offset) => group(nest(2, str("i64.load16_s") ++ memarg(align, offset, 1)))
        case i64.Load16U(align, offset) => group(nest(2, str("i64.load16_u") ++ memarg(align, offset, 1)))
        case i64.Load32S(align, offset) => group(nest(2, str("i64.load32_s") ++ memarg(align, offset, 2)))
        case i64.Load32U(align, offset) => group(nest(2, str("i64.load32_u") ++ memarg(align, offset, 2)))
        case i64.Store8(align, offset)  => group(nest(2, str("i64.store8") ++ memarg(align, offset, 0)))
        case i64.Store16(align, offset) => group(nest(2, str("i64.store16") ++ memarg(align, offset, 1)))
        case i64.Store32(align, offset) => group(nest(2, str("i64.store32") ++ memarg(align, offset, 2)))

        case f32.Const(v)             => str("f32.const ") ++ float(v)
        case f32.Abs()                => str("f32.abs")
        case f32.Neg()                => str("f32.neg")
        case f32.Sqrt()               => str("f32.sqrt")
        case f32.Ceil()               => str("f32.ceil")
        case f32.Floor()              => str("f32.floor")
        case f32.Trunc()              => str("f32.trunc")
        case f32.Nearest()            => str("f32.nearest")
        case f32.Add()                => str("f32.add")
        case f32.Sub()                => str("f32.sub")
        case f32.Mul()                => str("f32.mul")
        case f32.Div()                => str("f32.div")
        case f32.Min()                => str("f32.min")
        case f32.Max()                => str("f32.max")
        case f32.Copysign()           => str("f32.copysign")
        case f32.Eq()                 => str("f32.eq")
        case f32.Ne()                 => str("f32.ne")
        case f32.Lt()                 => str("f32.lt")
        case f32.Gt()                 => str("f32.gt")
        case f32.Le()                 => str("f32.le")
        case f32.Ge()                 => str("f32.ge")
        case f32.DemoteF64()          => str("f32.demote_f64")
        case f32.ConvertSI32()        => str("f32.convert_i32_s")
        case f32.ConvertUI32()        => str("f32.convert_i32_u")
        case f32.ConvertSI64()        => str("f32.convert_i64_s")
        case f32.ConvertUI64()        => str("f32.convert_i64_u")
        case f32.ReinterpretI32()     => str("f32.reinterpret_i32")
        case f32.Load(align, offset)  => group(nest(2, str("f32.load") ++ memarg(align, offset, 2)))
        case f32.Store(align, offset) => group(nest(2, str("f32.store") ++ memarg(align, offset, 2)))

        case f64.Const(v)             => str("f64.const ") ++ double(v)
        case f64.Abs()                => str("f64.abs")
        case f64.Neg()                => str("f64.neg")
        case f64.Sqrt()               => str("f64.sqrt")
        case f64.Ceil()               => str("f64.ceil")
        case f64.Floor()              => str("f64.floor")
        case f64.Trunc()              => str("f64.trunc")
        case f64.Nearest()            => str("f64.nearest")
        case f64.Add()                => str("f64.add")
        case f64.Sub()                => str("f64.sub")
        case f64.Mul()                => str("f64.mul")
        case f64.Div()                => str("f64.div")
        case f64.Min()                => str("f64.min")
        case f64.Max()                => str("f64.max")
        case f64.Copysign()           => str("f64.copysign")
        case f64.Eq()                 => str("f64.eq")
        case f64.Ne()                 => str("f64.ne")
        case f64.Lt()                 => str("f64.lt")
        case f64.Gt()                 => str("f64.gt")
        case f64.Le()                 => str("f64.le")
        case f64.Ge()                 => str("f64.ge")
        case f64.PromoteF32()         => str("f64.promote_f32")
        case f64.ConvertSI32()        => str("f64.convert_i32_s")
        case f64.ConvertUI32()        => str("f64.convert_i32_u")
        case f64.ConvertSI64()        => str("f64.convert_i64_s")
        case f64.ConvertUI64()        => str("f64.convert_i64_u")
        case f64.ReinterpretI64()     => str("f64.reinterpret_i64")
        case f64.Load(align, offset)  => group(nest(2, str("f64.load") ++ memarg(align, offset, 4)))
        case f64.Store(align, offset) => group(nest(2, str("f64.store") ++ memarg(align, offset, 4)))

        case Drop()   => str("drop")
        case Select() => str("select")

        case LocalGet(idx)  => nest(2, str("local.get") ++ idx.pretty)
        case LocalSet(idx)  => nest(2, str("local.set") ++ idx.pretty)
        case LocalTee(idx)  => nest(2, str("local.tee") ++ idx.pretty)
        case GlobalGet(idx) => nest(2, str("global.get") ++ idx.pretty)
        case GlobalSet(idx) => nest(2, str("global.set") ++ idx.pretty)

        case MemorySize() => str("memory.size")
        case MemoryGrow() => str("memory.grow")

        case Nop()         => str("nop")
        case Unreachable() => str("unreachable")

        case Block(lbl, tpe, instr, endlbl) =>
          group(
            nest(2, str("block") ++ lbl.pretty ++ tpe.pretty ++ line ++ instr.pretty) ++ line ++ str("end") ++ endlbl.pretty)

        case Loop(lbl, tpe, instr, endlbl) =>
          group(
            nest(2, str("loop") ++ lbl.pretty ++ tpe.pretty ++ line ++ instr.pretty) ++ line ++ str("end") ++ endlbl.pretty)

        case If(lbl, tpe, t, elselbl, e, endlbl) =>
          group(
            nest(2, str("if") ++ lbl.pretty ++ tpe.pretty ++ line ++ t.pretty) ++ line ++ nest(
              2,
              str("else") ++ elselbl.pretty ++ line ++ e.pretty) ++ line ++ str("end") ++ endlbl.pretty)

        case Br(lbl) => group(nest(2, str("br") ++ line ++ lbl.pretty))

        case BrIf(lbl) => group(nest(2, str("br_if") ++ line ++ lbl.pretty))

        case BrTable(lbls, lbl) => group(nest(2, str("br_table") ++ line ++ seq(line, lbls) ++ lbl.pretty))

        case Return() => str("return")

        case Call(idx) => group(nest(2, str("call") ++ idx.pretty))

        case CallIndirect(tu) => group(nest(2, str("call_indirect") ++ tu.pretty))

      }

  }

  private def long(l: Long): Doc =
    str(l.toString)

  private def float(f: Float): Doc =
    if (f.isNaN)
      str("nan")
    else if (f.isNegInfinity)
      str("-inf")
    else if (f.isPosInfinity)
      str("+inf")
    else
      str(f.toString)

  private def double(d: Double): Doc =
    if (d.isNaN)
      str("nan")
    else if (d.isNegInfinity)
      str("-inf")
    else if (d.isPosInfinity)
      str("+inf")
    else
      str(d.toString)

  private def memarg(align: Int, offset: Int, default: Int): Doc =
    (offset, align) match {
      case (0, `default`) => empty
      case (0, _)         => line ++ str("align=") ++ int(1 << align)
      case (_, `default`) => line ++ str("offset=") ++ int(offset)
      case (_, _)         => group(line ++ str("offset=") ++ int(offset) ++ line ++ str("align=") ++ int(1 << align))
    }

  def binop(op: Inst, d1: Doc, d2: Doc)(implicit E: Pretty[Expr]): Doc =
    group(nest(2, str("(") ++ op.pretty ++ line ++ fold(d1, empty) ++ line ++ fold(d2, empty) ++ str(")")))

  def unop(op: Inst, d: Doc)(implicit E: Pretty[Expr]): Doc =
    group(nest(2, str("(") ++ op.pretty ++ line ++ fold(d, empty) ++ str(")")))

  def fold(fst: Doc, rest: Doc): Doc =
    if (fst == empty)
      rest
    else if (rest == empty)
      if (fst.startsWith("("))
        fst
      else
        str("(") ++ fst ++ str(")")
    else
      group(nest(2, str("(") ++ fst ++ line ++ rest ++ str(")")))

  def foldedblock(b: Block)(implicit E: Pretty[Expr]): Doc = {
    val Block(lbl, tpe, instr, endlbl) = b
    group(nest(2, str("(block") ++ lbl.pretty ++ tpe.pretty ++ line ++ instr.pretty) ++ str(")"))
  }

  def foldedloop(l: Loop)(implicit E: Pretty[Expr]): Doc = {
    val Loop(lbl, tpe, instr, endlbl) = l
    group(nest(2, str("(loop") ++ lbl.pretty ++ tpe.pretty ++ line ++ instr.pretty) ++ str(")"))
  }

  def foldedif(i: If, cond: Doc)(implicit E: Pretty[Expr]): Doc = {
    val If(lbl, tpe, t, elselbl, e, endlbl) = i
    val hasElse = e.nonEmpty
    group(
      nest(2, str("(if") ++ lbl.pretty ++ tpe.pretty ++ line ++ cond) ++ line ++ group(nest(
        2,
        str("(then") ++ line ++ t.pretty) ++ str(")")) ++ (if (hasElse)
                                                             line ++ nest(2, str("(else") ++ line ++ e.pretty) ++ str(
                                                               ")")
                                                           else empty) ++ str(")"))

  }

  implicit object LimitsPretty extends Pretty[Limits] {
    def pretty(l: Limits): Doc =
      l match {
        case Limits(min, Some(max)) => int(min) ++ line ++ int(max)
        case Limits(min, None)      => int(min)
      }
  }

  implicit object ElemTypePretty extends Pretty[ElemType] {
    def pretty(et: ElemType): Doc =
      et match {
        case ElemType.FuncRef => str("funcref")
      }
  }

  implicit object TableTypePretty extends Pretty[TableType] {
    def pretty(tt: TableType): Doc =
      tt.limits.pretty ++ line ++ tt.elemtype.pretty
  }

  implicit object MemTypePretty extends Pretty[MemType] {
    def pretty(mt: MemType): Doc =
      mt.limits.pretty
  }

  implicit object GlobalTypePretty extends Pretty[GlobalType] {
    def pretty(gt: GlobalType): Doc =
      gt.mut match {
        case Mut.Const => gt.tpe.pretty
        case Mut.Var   => str("(mut ") ++ gt.tpe.pretty
      }
  }

  implicit object ImportDescPretty extends Pretty[ImportDesc] {
    def pretty(d: ImportDesc): Doc =
      d match {
        case ImportDesc.Func(id, tu)    => group(nest(2, str("(func") ++ id.pretty ++ tu.pretty) ++ str(")"))
        case ImportDesc.Table(id, tpe)  => group(nest(2, str("(table") ++ id.pretty ++ line ++ tpe.pretty) ++ str(")"))
        case ImportDesc.Memory(id, tpe) => group(nest(2, str("(memory") ++ id.pretty ++ line ++ tpe.pretty) ++ str(")"))
        case ImportDesc.Global(id, tpe) => group(nest(2, str("(global") ++ id.pretty ++ line ++ tpe.pretty) ++ str(")"))
      }
  }

  implicit object ExportDescPretty extends Pretty[ExportDesc] {
    def pretty(d: ExportDesc): Doc =
      d match {
        case ExportDesc.Func(id)   => str("(func") ++ id.pretty ++ str(")")
        case ExportDesc.Table(id)  => str("(table") ++ id.pretty ++ str(")")
        case ExportDesc.Memory(id) => str("(memory") ++ id.pretty ++ str(")")
        case ExportDesc.Global(id) => str("(global") ++ id.pretty ++ str(")")
      }
  }

  implicit object LocalPretty extends Pretty[Local] {
    def pretty(l: Local): Doc =
      str("(local") ++ l.id.pretty ++ space ++ l.tpe.pretty ++ str(")")
  }

  implicit object BytePretty extends Pretty[Byte] {
    def pretty(b: Byte): Doc =
      str((b & 0xff).toHexString)
  }

  implicit def FieldPretty(implicit E: Pretty[Expr]): Pretty[Field] = new Pretty[Field] {
    def pretty(f: Field): Doc =
      f match {
        case Type(id, pnames, tpe) =>
          group(
            nest(2,
                 str("(type") ++ id.pretty ++ line ++ group(
                   nest(2, str("(func")) ++ functype(pnames.zip(tpe.params), tpe.t) ++ str(")"))) ++ str(")"))
        case Import(mod, name, desc) =>
          group(
            nest(2,
                 str("(import") ++ line ++ str("\"") ++ str(mod) ++ str("\"") ++ line ++ str("\"") ++ str(name) ++ str(
                   "\"") ++ line ++ desc.pretty) ++ str(")"))
        case Function(id, tu, Seq(), is) =>
          group(nest(2, str("(func") ++ id.pretty ++ tu.pretty ++ line ++ is.pretty) ++ str(")"))
        case Function(id, tu, locals, is) =>
          group(nest(
            2,
            str("(func") ++ id.pretty ++ line ++ tu.pretty ++ line ++ seq(line, locals) ++ line ++ is.pretty) ++ str(
            ")"))
        case Table(id, tpe) =>
          group(nest(2, str("(table") ++ id.pretty ++ line ++ tpe.pretty) ++ str(")"))
        case Memory(id, tpe) =>
          group(nest(2, str("(memory") ++ id.pretty ++ line ++ tpe.pretty) ++ str(")"))
        case Global(id, tpe, init) =>
          group(nest(2, str("(global") ++ id.pretty ++ line ++ tpe.pretty ++ line ++ init.pretty) ++ str(")"))
        case Export(name, desc) =>
          group(
            nest(2, str("(export") ++ line ++ str("\"") ++ str(name) ++ str("\"") ++ line ++ desc.pretty) ++ str(")"))
        case StartFunc(idx) =>
          str("(start ") ++ idx.pretty ++ str(")")
        case Elem(table, offset, init) =>
          group(
            nest(2,
                 str("(elem") ++ table.pretty ++ line ++ group(
                   nest(2, str("(offset") ++ line ++ offset.pretty) ++ str(")")) ++ line ++ seq(line, init)) ++ str(
              ")"))
        case Data(mem, offset, init) =>
          group(
            nest(
              2,
              str("(data") ++ mem.pretty ++ line ++ group(nest(2, str("(offset") ++ line ++ offset.pretty) ++ str(")")) ++ line ++ str(
                "\"\\") ++ seq(str("\\"), init.toIndexedSeq.map(b => str(f"$b%02x")))
            ) ++ str("\"") ++ str(")"))
      }
  }

  implicit def ModulePretty(implicit E: Pretty[Expr]): Pretty[Module] = new Pretty[Module] {
    def pretty(m: Module): Doc =
      group(nest(2, str("(module") ++ m.id.pretty ++ line ++ seq(line, m.fields)) ++ str(")"))
  }
}
