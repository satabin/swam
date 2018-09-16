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
package parser

import unresolved._

import fastparse.noApi._
import fastparse.ext._

object Instructions {

  import Lexical._
  import white._
  import Types._

  private val label: P[Id] =
    P(id.!.?.map {
      case Some(id) => SomeId(id)
      case None     => NoId
    })

  private val blockinstr: P[Int => Inst] =
    P(
      (word("block") ~/ label ~ resulttype ~ instrs ~ word("end") ~ label)
        .map((Block.apply _).tupled)
        | (word("loop") ~/ label ~ resulttype ~ instrs ~/ word("end") ~ label)
          .map((Loop.apply _).tupled)
        | (word("if") ~/ label ~ resulttype ~ instrs ~ (word("else") ~/ label ~ instrs).? ~ word("end") ~ label)
          .map {
            case (lbl, tpe, theni, Some((elselbl, elsei)), endlbl) =>
              If(lbl, tpe, theni, elselbl, elsei, endlbl) _
            case (lbl, tpe, theni, None, endlbl) =>
              If(lbl, tpe, theni, NoId, Seq.empty, endlbl) _
          })

  private val plaininstr: P[Int => Inst] =
    P(
      (StringInV(Map(
        "unreachable" -> Unreachable() _,
        "nop" -> Nop() _,
        "return" -> Return() _,
        "drop" -> Drop() _,
        "select" -> Select() _,
        "memory.size" -> MemorySize() _,
        "memory.grow" -> MemoryGrow() _,
        "i32.clz" -> i32.Clz() _,
        "i32.ctz" -> i32.Ctz() _,
        "i32.popcnt" -> i32.Popcnt() _,
        "i32.add" -> i32.Add() _,
        "i32.sub" -> i32.Sub() _,
        "i32.mul" -> i32.Mul() _,
        "i32.div_s" -> i32.DivS() _,
        "i32.div_u" -> i32.DivU() _,
        "i32.rem_s" -> i32.RemS() _,
        "i32.rem_u" -> i32.RemU() _,
        "i32.and" -> i32.And() _,
        "i32.or" -> i32.Or() _,
        "i32.xor" -> i32.Xor() _,
        "i32.shl" -> i32.Shl() _,
        "i32.shr_s" -> i32.ShrS() _,
        "i32.shr_u" -> i32.ShrU() _,
        "i32.rotl" -> i32.Rotl() _,
        "i32.rotr" -> i32.Rotr() _,
        "i64.clz" -> i64.Clz() _,
        "i64.ctz" -> i64.Ctz() _,
        "i64.popcnt" -> i64.Popcnt() _,
        "i64.add" -> i64.Add() _,
        "i64.sub" -> i64.Sub() _,
        "i64.mul" -> i64.Mul() _,
        "i64.div_s" -> i64.DivS() _,
        "i64.div_u" -> i64.DivU() _,
        "i64.rem_s" -> i64.RemS() _,
        "i64.rem_u" -> i64.RemU() _,
        "i64.and" -> i64.And() _,
        "i64.or" -> i64.Or() _,
        "i64.xor" -> i64.Xor() _,
        "i64.shl" -> i64.Shl() _,
        "i64.shr_s" -> i64.ShrS() _,
        "i64.shr_u" -> i64.ShrU() _,
        "i64.rotl" -> i64.Rotl() _,
        "i64.rotr" -> i64.Rotr() _,
        "f32.abs" -> f32.Abs() _,
        "f32.neg" -> f32.Neg() _,
        "f32.ceil" -> f32.Ceil() _,
        "f32.floor" -> f32.Floor() _,
        "f32.trunc" -> f32.Trunc() _,
        "f32.nearest" -> f32.Nearest() _,
        "f32.sqrt" -> f32.Sqrt() _,
        "f32.add" -> f32.Add() _,
        "f32.sub" -> f32.Sub() _,
        "f32.mul" -> f32.Mul() _,
        "f32.div" -> f32.Div() _,
        "f32.min" -> f32.Min() _,
        "f32.max" -> f32.Max() _,
        "f32.copysign" -> f32.Copysign() _,
        "f64.abs" -> f64.Abs() _,
        "f64.neg" -> f64.Neg() _,
        "f64.ceil" -> f64.Ceil() _,
        "f64.floor" -> f64.Floor() _,
        "f64.trunc" -> f64.Trunc() _,
        "f64.nearest" -> f64.Nearest() _,
        "f64.sqrt" -> f64.Sqrt() _,
        "f64.add" -> f64.Add() _,
        "f64.sub" -> f64.Sub() _,
        "f64.mul" -> f64.Mul() _,
        "f64.div" -> f64.Div() _,
        "f64.min" -> f64.Min() _,
        "f64.max" -> f64.Max() _,
        "f64.copysign" -> f64.Copysign() _,
        "i32.eqz" -> i32.Eqz() _,
        "i32.eq" -> i32.Eq() _,
        "i32.ne" -> i32.Ne() _,
        "i32.lt_s" -> i32.LtS() _,
        "i32.lt_u" -> i32.LtU() _,
        "i32.gt_s" -> i32.GtS() _,
        "i32.gt_u" -> i32.GtU() _,
        "i32.le_s" -> i32.LeS() _,
        "i32.le_u" -> i32.LeU() _,
        "i32.ge_s" -> i32.GeS() _,
        "i32.ge_u" -> i32.GeU() _,
        "i64.eqz" -> i64.Eqz() _,
        "i64.eq" -> i64.Eq() _,
        "i64.ne" -> i64.Ne() _,
        "i64.lt_s" -> i64.LtS() _,
        "i64.lt_u" -> i64.LtU() _,
        "i64.gt_s" -> i64.GtS() _,
        "i64.gt_u" -> i64.GtU() _,
        "i64.le_s" -> i64.LeS() _,
        "i64.le_u" -> i64.LeU() _,
        "i64.ge_s" -> i64.GeS() _,
        "i64.ge_u" -> i64.GeU() _,
        "f32.eq" -> f32.Eq() _,
        "f32.ne" -> f32.Ne() _,
        "f32.lt" -> f32.Lt() _,
        "f32.gt" -> f32.Gt() _,
        "f32.le" -> f32.Le() _,
        "f32.ge" -> f32.Ge() _,
        "f64.eq" -> f64.Eq() _,
        "f64.ne" -> f64.Ne() _,
        "f64.lt" -> f64.Lt() _,
        "f64.gt" -> f64.Gt() _,
        "f64.le" -> f64.Le() _,
        "f64.ge" -> f64.Ge() _,
        "i32.wrap/i64" -> i32.WrapI64() _,
        "i32.trunc_s/f32" -> i32.TruncSF32() _,
        "i32.trunc_u/f32" -> i32.TruncUF32() _,
        "i32.trunc_s/f64" -> i32.TruncSF64() _,
        "i32.trunc_u/f64" -> i32.TruncUF64() _,
        "i64.extend_s/i32" -> i64.ExtendSI32() _,
        "i64.extend_u/i32" -> i64.ExtendUI32() _,
        "i64.trunc_s/f32" -> i64.TruncSF32() _,
        "i64.trunc_u/f32" -> i64.TruncUF32() _,
        "i64.trunc_s/f64" -> i64.TruncSF64() _,
        "i64.trunc_u/f64" -> i64.TruncUF64() _,
        "f32.convert_s/i32" -> f32.ConvertSI32() _,
        "f32.convert_u/i32" -> f32.ConvertUI32() _,
        "f32.convert_s/i64" -> f32.ConvertSI64() _,
        "f32.convert_u/i64" -> f32.ConvertUI64() _,
        "f32.demote/f64" -> f32.DemoteF64() _,
        "f64.convert_s/i32" -> f64.ConvertSI32() _,
        "f64.convert_u/i32" -> f64.ConvertUI32() _,
        "f64.convert_s/i64" -> f64.ConvertSI64() _,
        "f64.convert_u/i64" -> f64.ConvertUI64() _,
        "f64.promote/f32" -> f64.PromoteF32() _,
        "i32.reinterpret/f32" -> i32.ReinterpretF32() _,
        "i64.reinterpret/f64" -> i64.ReinterpretF64() _,
        "f32.reinterpret/i32" -> f32.ReinterpretI32() _,
        "f64.reinterpret/i64" -> f64.ReinterpretI64() _,
      )) ~~ !idchar)
        | word("br") ~/ index.map(Br(_) _)
        | word("br_if") ~/ index.map(BrIf(_) _)
        | word("br_table") ~/ index
          .rep(min = 1)
          .map(ls => BrTable(ls.init.toVector, ls.last) _)
        | word("call") ~/ index.map(Call(_) _)
        | word("call_indirect") ~/ typeuse.map(CallIndirect(_) _)
        | word("get_local") ~/ index.map(GetLocal(_) _)
        | word("set_local") ~/ index.map(SetLocal(_) _)
        | word("tee_local") ~/ index.map(TeeLocal(_) _)
        | word("get_global") ~/ index.map(GetGlobal(_) _)
        | word("set_global") ~/ index.map(SetGlobal(_) _)
        | word("i32.load") ~/ memarg4.map((i32.Load.apply _).tupled)
        | word("i64.load") ~/ memarg8.map((i64.Load.apply _).tupled)
        | word("f32.load") ~/ memarg4.map((f32.Load.apply _).tupled)
        | word("f64.load") ~/ memarg8.map((f64.Load.apply _).tupled)
        | word("i32.load8_s") ~/ memarg1.map((i32.Load8S.apply _).tupled)
        | word("i32.load8_u") ~/ memarg1.map((i32.Load8U.apply _).tupled)
        | word("i32.load16_s") ~/ memarg2.map((i32.Load16S.apply _).tupled)
        | word("i32.load16_u") ~/ memarg2.map((i32.Load16U.apply _).tupled)
        | word("i64.load8_s") ~/ memarg1.map((i64.Load8S.apply _).tupled)
        | word("i64.load8_u") ~/ memarg1.map((i64.Load8U.apply _).tupled)
        | word("i64.load16_s") ~/ memarg2.map((i64.Load16S.apply _).tupled)
        | word("i64.load16_u") ~/ memarg2.map((i64.Load16U.apply _).tupled)
        | word("i64.load32_s") ~/ memarg4.map((i64.Load32S.apply _).tupled)
        | word("i64.load32_u") ~/ memarg4.map((i64.Load32U.apply _).tupled)
        | word("i32.store") ~/ memarg4.map((i32.Store.apply _).tupled)
        | word("i64.store") ~/ memarg8.map((i64.Store.apply _).tupled)
        | word("f32.store") ~/ memarg4.map((f32.Store.apply _).tupled)
        | word("f64.store") ~/ memarg8.map((f64.Store.apply _).tupled)
        | word("i32.store8") ~/ memarg1.map((i32.Store8.apply _).tupled)
        | word("i32.store16") ~/ memarg2.map((i32.Store16.apply _).tupled)
        | word("i64.store8") ~/ memarg1.map((i64.Store8.apply _).tupled)
        | word("i64.store16") ~/ memarg2.map((i64.Store16.apply _).tupled)
        | word("i64.store32") ~/ memarg4.map((i64.Store32.apply _).tupled)
        | word("i32.const") ~/ int32.map(i32.Const(_) _)
        | word("i64.const") ~/ int64.map(i64.Const(_) _)
        | word("f32.const") ~/ float32.map(f32.Const(_) _)
        | word("f64.const") ~/ float64.map(f64.Const(_) _)
    )

  private def memarg(n: Int): P[(Int, Int)] =
    P(("offset=" ~~ uint32).?.map(_.getOrElse(0)) ~ ("align=" ~~ uint32).?.map(a => log2(a.getOrElse(n)))).map(_.swap)

  private val memarg1 = memarg(1)
  private val memarg2 = memarg(2)
  private val memarg4 = memarg(4)
  private val memarg8 = memarg(8)

  private def log2(n: Int) =
    31 - Integer.numberOfLeadingZeros(n)

  val instr: P[Inst] =
    P(Index ~ (blockinstr | plaininstr)).map { case (idx, i) => i(idx) }

  val foldedinstr: P[Seq[Inst]] =
    P(
      ("(" ~/ (Index ~ (NoCut(plaininstr ~ foldedinstr.rep.map(_.flatten))
        | ((word("block") ~/ label ~ resulttype ~ instrs)
          .map { case (lbl, tpe, is) => Block(lbl, tpe, is, NoId) _ } ~ PassWith(Seq.empty))
        | ((word("loop") ~/ label ~ resulttype ~ instrs)
          .map { case (lbl, tpe, is) => Loop(lbl, tpe, is, NoId) _ } ~ PassWith(Seq.empty))
        | (word("if") ~/ label ~ resulttype ~ NoCut(foldedinstr).rep
          .map(_.flatten) ~ "(" ~ word("then") ~ instrs ~ ")" ~ ("(" ~ word("else") ~/ instrs ~ ")").?)
          .map {
            case (lbl, tpe, is, theni, Some(elsei)) =>
              (If(lbl, tpe, theni, NoId, elsei, NoId) _, is)
            case (lbl, tpe, is, theni, None) =>
              (If(lbl, tpe, theni, NoId, Seq.empty, NoId) _, is)
          })) ~ ")").map {
        case (idx, (i, is)) => is :+ i(idx)
      })

  val instrs: P[Seq[Inst]] =
    P((instr.rep(min = 1) | foldedinstr).rep.map(_.flatten))

  val expr: P[Expr] =
    instrs

}
