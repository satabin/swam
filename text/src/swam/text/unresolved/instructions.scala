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

sealed trait Inst {
  val pos: Int
}

sealed abstract class Unop(val tpe: ValType) extends Inst
object Unop {
  def unapply(unop: Unop): Option[ValType] =
    Some(unop.tpe)
}

sealed abstract class Binop(val tpe: ValType) extends Inst
object Binop {
  def unapply(binop: Binop): Option[ValType] =
    Some(binop.tpe)
}

sealed abstract class Testop(val tpe: ValType) extends Inst
object Testop {
  def unapply(testop: Testop): Option[ValType] =
    Some(testop.tpe)
}

sealed abstract class Relop(val tpe: ValType) extends Inst
object Relop {
  def unapply(relop: Relop): Option[ValType] =
    Some(relop.tpe)
}

sealed abstract class Convertop(val from: ValType, val to: ValType) extends Inst
object Convertop {
  def unapply(op: Convertop): Option[(ValType, ValType)] =
    Some(op.from -> op.to)
}

sealed abstract class Miscop(val index: Byte) extends Inst
object Miscop {
  def unapply(op: Miscop): Option[Byte] =
    Some(op.index)
}

sealed abstract class SatConvertop(val from: ValType, val to: ValType, index: Byte) extends Miscop(index)
object SatConvertop {
  def unapply(op: SatConvertop): Option[(ValType, ValType)] =
    Some(op.from -> op.to)
}

sealed abstract class IUnop(tpe: ValType) extends Unop(tpe)

sealed abstract class IBinop(tpe: ValType) extends Binop(tpe)

sealed abstract class ITestop(tpe: ValType) extends Testop(tpe)

sealed abstract class IRelop(tpe: ValType) extends Relop(tpe)

sealed abstract class FUnop(tpe: ValType) extends Unop(tpe)

sealed abstract class FBinop(tpe: ValType) extends Binop(tpe)

sealed abstract class FRelop(tpe: ValType) extends Relop(tpe)

sealed trait MemoryInst extends Inst {
  val offset: Int
  val align: Int
}

object MemoryInst {
  def unapply(inst: MemoryInst): Option[(Int, Int)] =
    Some(inst.align -> inst.offset)
}

sealed abstract class LoadInst(val tpe: ValType) extends MemoryInst
object Load {
  def unapply(op: LoadInst): Option[(ValType, Int, Int)] =
    Some((op.tpe, op.align, op.offset))
}

sealed abstract class LoadNInst(val tpe: ValType, val n: Int) extends MemoryInst
object LoadN {
  def unapply(op: LoadNInst): Option[(ValType, Int, Int, Int)] =
    Some((op.tpe, op.n, op.align, op.offset))
}

sealed abstract class StoreInst(val tpe: ValType) extends MemoryInst
object Store {
  def unapply(op: StoreInst): Option[(ValType, Int, Int)] =
    Some((op.tpe, op.align, op.offset))
}

sealed abstract class StoreNInst(val tpe: ValType, val n: Int) extends MemoryInst
object StoreN {
  def unapply(op: StoreNInst): Option[(ValType, Int, Int, Int)] =
    Some((op.tpe, op.n, op.align, op.offset))
}

object i32 {

  case class Const(v: Int)(val pos: Int) extends Inst

  case class Clz()(val pos: Int) extends IUnop(ValType.I32)
  case class Ctz()(val pos: Int) extends IUnop(ValType.I32)
  case class Popcnt()(val pos: Int) extends IUnop(ValType.I32)
  case class Extend8S()(val pos: Int) extends IUnop(ValType.I32)
  case class Extend16S()(val pos: Int) extends IUnop(ValType.I32)

  case class Add()(val pos: Int) extends IBinop(ValType.I32)
  case class Sub()(val pos: Int) extends IBinop(ValType.I32)
  case class Mul()(val pos: Int) extends IBinop(ValType.I32)
  case class DivS()(val pos: Int) extends IBinop(ValType.I32)
  case class DivU()(val pos: Int) extends IBinop(ValType.I32)
  case class RemS()(val pos: Int) extends IBinop(ValType.I32)
  case class RemU()(val pos: Int) extends IBinop(ValType.I32)
  case class And()(val pos: Int) extends IBinop(ValType.I32)
  case class Or()(val pos: Int) extends IBinop(ValType.I32)
  case class Xor()(val pos: Int) extends IBinop(ValType.I32)
  case class Shl()(val pos: Int) extends IBinop(ValType.I32)
  case class ShrS()(val pos: Int) extends IBinop(ValType.I32)
  case class ShrU()(val pos: Int) extends IBinop(ValType.I32)
  case class Rotl()(val pos: Int) extends IBinop(ValType.I32)
  case class Rotr()(val pos: Int) extends IBinop(ValType.I32)

  case class Eqz()(val pos: Int) extends ITestop(ValType.I32)

  case class Eq()(val pos: Int) extends IRelop(ValType.I32)
  case class Ne()(val pos: Int) extends IRelop(ValType.I32)
  case class LtS()(val pos: Int) extends IRelop(ValType.I32)
  case class LtU()(val pos: Int) extends IRelop(ValType.I32)
  case class GtS()(val pos: Int) extends IRelop(ValType.I32)
  case class GtU()(val pos: Int) extends IRelop(ValType.I32)
  case class LeS()(val pos: Int) extends IRelop(ValType.I32)
  case class LeU()(val pos: Int) extends IRelop(ValType.I32)
  case class GeS()(val pos: Int) extends IRelop(ValType.I32)
  case class GeU()(val pos: Int) extends IRelop(ValType.I32)

  case class WrapI64()(val pos: Int) extends Convertop(ValType.I64, ValType.I32)

  case class TruncSF32()(val pos: Int) extends Convertop(ValType.F32, ValType.I32)
  case class TruncUF32()(val pos: Int) extends Convertop(ValType.F32, ValType.I32)
  case class TruncSF64()(val pos: Int) extends Convertop(ValType.F64, ValType.I32)
  case class TruncUF64()(val pos: Int) extends Convertop(ValType.F64, ValType.I32)

  case class TruncSatSF32()(val pos: Int) extends SatConvertop(ValType.F32, ValType.I32, 0x00)
  case class TruncSatUF32()(val pos: Int) extends SatConvertop(ValType.F32, ValType.I32, 0x01)
  case class TruncSatSF64()(val pos: Int) extends SatConvertop(ValType.F64, ValType.I32, 0x02)
  case class TruncSatUF64()(val pos: Int) extends SatConvertop(ValType.F64, ValType.I32, 0x03)

  case class ReinterpretF32()(val pos: Int) extends Convertop(ValType.F32, ValType.I32)

  case class Load(align: Int, offset: Int)(val pos: Int) extends LoadInst(ValType.I32)
  case class Store(align: Int, offset: Int)(val pos: Int) extends StoreInst(ValType.I32)

  case class Load8S(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I32, 8)
  case class Load8U(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I32, 8)
  case class Load16S(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I32, 16)
  case class Load16U(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I32, 16)

  case class Store8(align: Int, offset: Int)(val pos: Int) extends StoreNInst(ValType.I32, 8)
  case class Store16(align: Int, offset: Int)(val pos: Int) extends StoreNInst(ValType.I32, 16)

}

object i64 {

  case class Const(v: Long)(val pos: Int) extends Inst

  case class Clz()(val pos: Int) extends IUnop(ValType.I64)
  case class Ctz()(val pos: Int) extends IUnop(ValType.I64)
  case class Popcnt()(val pos: Int) extends IUnop(ValType.I64)
  case class Extend8S()(val pos: Int) extends IUnop(ValType.I64)
  case class Extend16S()(val pos: Int) extends IUnop(ValType.I64)
  case class Extend32S()(val pos: Int) extends IUnop(ValType.I64)

  case class Add()(val pos: Int) extends IBinop(ValType.I64)
  case class Sub()(val pos: Int) extends IBinop(ValType.I64)
  case class Mul()(val pos: Int) extends IBinop(ValType.I64)
  case class DivS()(val pos: Int) extends IBinop(ValType.I64)
  case class DivU()(val pos: Int) extends IBinop(ValType.I64)
  case class RemS()(val pos: Int) extends IBinop(ValType.I64)
  case class RemU()(val pos: Int) extends IBinop(ValType.I64)
  case class And()(val pos: Int) extends IBinop(ValType.I64)
  case class Or()(val pos: Int) extends IBinop(ValType.I64)
  case class Xor()(val pos: Int) extends IBinop(ValType.I64)
  case class Shl()(val pos: Int) extends IBinop(ValType.I64)
  case class ShrS()(val pos: Int) extends IBinop(ValType.I64)
  case class ShrU()(val pos: Int) extends IBinop(ValType.I64)
  case class Rotl()(val pos: Int) extends IBinop(ValType.I64)
  case class Rotr()(val pos: Int) extends IBinop(ValType.I64)

  case class Eqz()(val pos: Int) extends ITestop(ValType.I64)

  case class Eq()(val pos: Int) extends IRelop(ValType.I64)
  case class Ne()(val pos: Int) extends IRelop(ValType.I64)
  case class LtS()(val pos: Int) extends IRelop(ValType.I64)
  case class LtU()(val pos: Int) extends IRelop(ValType.I64)
  case class GtS()(val pos: Int) extends IRelop(ValType.I64)
  case class GtU()(val pos: Int) extends IRelop(ValType.I64)
  case class LeS()(val pos: Int) extends IRelop(ValType.I64)
  case class LeU()(val pos: Int) extends IRelop(ValType.I64)
  case class GeS()(val pos: Int) extends IRelop(ValType.I64)
  case class GeU()(val pos: Int) extends IRelop(ValType.I64)

  case class ExtendSI32()(val pos: Int) extends Convertop(ValType.I32, ValType.I64)
  case class ExtendUI32()(val pos: Int) extends Convertop(ValType.I32, ValType.I64)

  case class TruncSF32()(val pos: Int) extends Convertop(ValType.F32, ValType.I64)
  case class TruncUF32()(val pos: Int) extends Convertop(ValType.F32, ValType.I64)
  case class TruncSF64()(val pos: Int) extends Convertop(ValType.F64, ValType.I64)
  case class TruncUF64()(val pos: Int) extends Convertop(ValType.F64, ValType.I64)

  case class TruncSatSF32()(val pos: Int) extends SatConvertop(ValType.F32, ValType.I64, 0x04)
  case class TruncSatUF32()(val pos: Int) extends SatConvertop(ValType.F32, ValType.I64, 0x05)
  case class TruncSatSF64()(val pos: Int) extends SatConvertop(ValType.F64, ValType.I64, 0x06)
  case class TruncSatUF64()(val pos: Int) extends SatConvertop(ValType.F64, ValType.I64, 0x07)

  case class ReinterpretF64()(val pos: Int) extends Convertop(ValType.F64, ValType.I64)

  case class Load(align: Int, offset: Int)(val pos: Int) extends LoadInst(ValType.I64)
  case class Store(align: Int, offset: Int)(val pos: Int) extends StoreInst(ValType.I64)

  case class Load8S(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I64, 8)
  case class Load8U(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I64, 8)
  case class Load16S(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I64, 16)
  case class Load16U(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I64, 16)
  case class Load32S(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I64, 32)
  case class Load32U(align: Int, offset: Int)(val pos: Int) extends LoadNInst(ValType.I64, 32)

  case class Store8(align: Int, offset: Int)(val pos: Int) extends StoreNInst(ValType.I64, 8)
  case class Store16(align: Int, offset: Int)(val pos: Int) extends StoreNInst(ValType.I64, 16)
  case class Store32(align: Int, offset: Int)(val pos: Int) extends StoreNInst(ValType.I64, 32)

}

object f32 {

  case class Const(v: Float)(val pos: Int) extends Inst

  case class Abs()(val pos: Int) extends FUnop(ValType.F32)
  case class Neg()(val pos: Int) extends FUnop(ValType.F32)
  case class Sqrt()(val pos: Int) extends FUnop(ValType.F32)
  case class Ceil()(val pos: Int) extends FUnop(ValType.F32)
  case class Floor()(val pos: Int) extends FUnop(ValType.F32)
  case class Trunc()(val pos: Int) extends FUnop(ValType.F32)
  case class Nearest()(val pos: Int) extends FUnop(ValType.F32)

  case class Add()(val pos: Int) extends FBinop(ValType.F32)
  case class Sub()(val pos: Int) extends FBinop(ValType.F32)
  case class Mul()(val pos: Int) extends FBinop(ValType.F32)
  case class Div()(val pos: Int) extends FBinop(ValType.F32)
  case class Min()(val pos: Int) extends FBinop(ValType.F32)
  case class Max()(val pos: Int) extends FBinop(ValType.F32)
  case class Copysign()(val pos: Int) extends FBinop(ValType.F32)

  case class Eq()(val pos: Int) extends FRelop(ValType.F32)
  case class Ne()(val pos: Int) extends FRelop(ValType.F32)
  case class Lt()(val pos: Int) extends FRelop(ValType.F32)
  case class Gt()(val pos: Int) extends FRelop(ValType.F32)
  case class Le()(val pos: Int) extends FRelop(ValType.F32)
  case class Ge()(val pos: Int) extends FRelop(ValType.F32)

  case class DemoteF64()(val pos: Int) extends Convertop(ValType.F64, ValType.F32)

  case class ConvertSI32()(val pos: Int) extends Convertop(ValType.I32, ValType.F32)
  case class ConvertUI32()(val pos: Int) extends Convertop(ValType.I32, ValType.F32)
  case class ConvertSI64()(val pos: Int) extends Convertop(ValType.I64, ValType.F32)
  case class ConvertUI64()(val pos: Int) extends Convertop(ValType.I64, ValType.F32)

  case class ReinterpretI32()(val pos: Int) extends Convertop(ValType.I32, ValType.F32)

  case class Load(align: Int, offset: Int)(val pos: Int) extends LoadInst(ValType.I32)
  case class Store(align: Int, offset: Int)(val pos: Int) extends MemoryInst

}

object f64 {

  case class Const(v: Double)(val pos: Int) extends Inst

  case class Abs()(val pos: Int) extends FUnop(ValType.F64)
  case class Neg()(val pos: Int) extends FUnop(ValType.F64)
  case class Sqrt()(val pos: Int) extends FUnop(ValType.F64)
  case class Ceil()(val pos: Int) extends FUnop(ValType.F64)
  case class Floor()(val pos: Int) extends FUnop(ValType.F64)
  case class Trunc()(val pos: Int) extends FUnop(ValType.F64)
  case class Nearest()(val pos: Int) extends FUnop(ValType.F64)

  case class Add()(val pos: Int) extends FBinop(ValType.F64)
  case class Sub()(val pos: Int) extends FBinop(ValType.F64)
  case class Mul()(val pos: Int) extends FBinop(ValType.F64)
  case class Div()(val pos: Int) extends FBinop(ValType.F64)
  case class Min()(val pos: Int) extends FBinop(ValType.F64)
  case class Max()(val pos: Int) extends FBinop(ValType.F64)
  case class Copysign()(val pos: Int) extends FBinop(ValType.F64)

  case class Eq()(val pos: Int) extends FRelop(ValType.F64)
  case class Ne()(val pos: Int) extends FRelop(ValType.F64)
  case class Lt()(val pos: Int) extends FRelop(ValType.F64)
  case class Gt()(val pos: Int) extends FRelop(ValType.F64)
  case class Le()(val pos: Int) extends FRelop(ValType.F64)
  case class Ge()(val pos: Int) extends FRelop(ValType.F64)

  case class PromoteF32()(val pos: Int) extends Convertop(ValType.F32, ValType.F64)

  case class ConvertSI32()(val pos: Int) extends Convertop(ValType.I32, ValType.F64)
  case class ConvertUI32()(val pos: Int) extends Convertop(ValType.I32, ValType.F64)
  case class ConvertSI64()(val pos: Int) extends Convertop(ValType.I64, ValType.F64)
  case class ConvertUI64()(val pos: Int) extends Convertop(ValType.I64, ValType.F64)

  case class ReinterpretI64()(val pos: Int) extends Convertop(ValType.I64, ValType.F64)

  case class Load(align: Int, offset: Int)(val pos: Int) extends LoadInst(ValType.I32)
  case class Store(align: Int, offset: Int)(val pos: Int) extends MemoryInst

}

case class Drop()(val pos: Int) extends Inst
case class Select()(val pos: Int) extends Inst

case class LocalGet(idx: Index)(val pos: Int) extends Inst
case class LocalSet(idx: Index)(val pos: Int) extends Inst
case class LocalTee(idx: Index)(val pos: Int) extends Inst
case class GlobalGet(idx: Index)(val pos: Int) extends Inst
case class GlobalSet(idx: Index)(val pos: Int) extends Inst

case class MemorySize()(val pos: Int) extends Inst
case class MemoryGrow()(val pos: Int) extends Inst

case class Nop()(val pos: Int) extends Inst
case class Unreachable()(val pos: Int) extends Inst
case class Block(label: Id, tpe: TypeUse, instr: Seq[Inst], endlabel: Id)(val pos: Int) extends Inst
case class Loop(label: Id, tpe: TypeUse, instr: Seq[Inst], endlabel: Id)(val pos: Int) extends Inst
case class If(label: Id, tpe: TypeUse, thenInstr: Seq[Inst], elselabel: Id, elseInstr: Seq[Inst], endlabel: Id)(
    val pos: Int)
    extends Inst
case class Br(lbl: Index)(val pos: Int) extends Inst
case class BrIf(lbl: Index)(val pos: Int) extends Inst
case class BrTable(table: Vector[Index], lbl: Index)(val pos: Int) extends Inst
case class Return()(val pos: Int) extends Inst
case class Call(funcidx: Index)(val pos: Int) extends Inst
case class CallIndirect(typeuse: TypeUse)(val pos: Int) extends Inst

case class TypeUse(tpe: Option[Index], params: Vector[Param], results: Vector[ValType])
