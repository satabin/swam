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

sealed abstract class Inst(val opcode: OpCode)

sealed abstract class Unop(val tpe: ValType, opcode: OpCode) extends Inst(opcode)
object Unop {
  def unapply(unop: Unop): Option[ValType] =
    Some(unop.tpe)
}

sealed abstract class Binop(val tpe: ValType, opcode: OpCode) extends Inst(opcode)
object Binop {
  def unapply(binop: Binop): Option[ValType] =
    Some(binop.tpe)
}

sealed abstract class Testop(val tpe: ValType, opcode: OpCode) extends Inst(opcode)
object Testop {
  def unapply(testop: Testop): Option[ValType] =
    Some(testop.tpe)
}

sealed abstract class Relop(val tpe: ValType, opcode: OpCode) extends Inst(opcode)
object Relop {
  def unapply(relop: Relop): Option[ValType] =
    Some(relop.tpe)
}

sealed abstract class Convertop(val from: ValType, val to: ValType, opcode: OpCode) extends Inst(opcode)
object Convertop {
  def unapply(op: Convertop): Option[(ValType, ValType)] =
    Some(op.from -> op.to)
}

sealed abstract class IUnop(tpe: ValType, opcode: OpCode) extends Unop(tpe, opcode)

sealed abstract class IBinop(tpe: ValType, opcode: OpCode) extends Binop(tpe, opcode)

sealed abstract class ITestop(tpe: ValType, opcode: OpCode) extends Testop(tpe, opcode)

sealed abstract class IRelop(tpe: ValType, opcode: OpCode) extends Relop(tpe, opcode)

sealed abstract class FUnop(tpe: ValType, opcode: OpCode) extends Unop(tpe, opcode)

sealed abstract class FBinop(tpe: ValType, opcode: OpCode) extends Binop(tpe, opcode)

sealed abstract class FRelop(tpe: ValType, opcode: OpCode) extends Relop(tpe, opcode)

sealed abstract class MemoryInst(opcode: OpCode) extends Inst(opcode) {
  val offset: Int
  val align: Int
}

object MemoryInst {
  def unapply(inst: MemoryInst): Option[(Int, Int)] =
    Some(inst.offset -> inst.align)
}

sealed abstract class LoadInst(val tpe: ValType, opcode: OpCode) extends MemoryInst(opcode)
object Load {
  def unapply(op: LoadInst): Option[(ValType, Int, Int)] =
    Some((op.tpe, op.offset, op.align))
}

sealed abstract class LoadNInst(val tpe: ValType, val n: Int, opcode: OpCode) extends MemoryInst(opcode)
object LoadN {
  def unapply(op: LoadNInst): Option[(ValType, Int, Int, Int)] =
    Some((op.tpe, op.n, op.offset, op.align))
}

sealed abstract class StoreInst(val tpe: ValType, opcode: OpCode) extends MemoryInst(opcode)
object Store {
  def unapply(op: StoreInst): Option[(ValType, Int, Int)] =
    Some((op.tpe, op.offset, op.align))
}

sealed abstract class StoreNInst(val tpe: ValType, val n: Int, opcode: OpCode) extends MemoryInst(opcode)
object StoreN {
  def unapply(op: StoreNInst): Option[(ValType, Int, Int, Int)] =
    Some((op.tpe, op.n, op.offset, op.align))
}

sealed abstract class VarInst(opcode: OpCode) extends Inst(opcode) {
  val idx: Int
}

object VarInst {
  def unapply(op: VarInst): Option[Int] =
    Some(op.idx)
}

object i32 {

  case class Const(v: Int) extends Inst(OpCode.I32Const)

  case object Clz extends IUnop(ValType.I32, OpCode.I32Clz)
  case object Ctz extends IUnop(ValType.I32, OpCode.I32Ctz)
  case object Popcnt extends IUnop(ValType.I32, OpCode.I32Popcnt)

  case object Add extends IBinop(ValType.I32, OpCode.I32Add)
  case object Sub extends IBinop(ValType.I32, OpCode.I32Sub)
  case object Mul extends IBinop(ValType.I32, OpCode.I32Mul)
  case object DivS extends IBinop(ValType.I32, OpCode.I32DivS)
  case object DivU extends IBinop(ValType.I32, OpCode.I32DivU)
  case object RemS extends IBinop(ValType.I32, OpCode.I32RemS)
  case object RemU extends IBinop(ValType.I32, OpCode.I32RemU)
  case object And extends IBinop(ValType.I32, OpCode.I32And)
  case object Or extends IBinop(ValType.I32, OpCode.I32Or)
  case object Xor extends IBinop(ValType.I32, OpCode.I32Xor)
  case object Shl extends IBinop(ValType.I32, OpCode.I32Shl)
  case object ShrS extends IBinop(ValType.I32, OpCode.I32ShrS)
  case object ShrU extends IBinop(ValType.I32, OpCode.I32ShrU)
  case object Rotl extends IBinop(ValType.I32, OpCode.I32Rotl)
  case object Rotr extends IBinop(ValType.I32, OpCode.I32Rotr)

  case object Eqz extends ITestop(ValType.I32, OpCode.I32Eqz)

  case object Eq extends IRelop(ValType.I32, OpCode.I32Eq)
  case object Ne extends IRelop(ValType.I32, OpCode.I32Ne)
  case object LtS extends IRelop(ValType.I32, OpCode.I32LtS)
  case object LtU extends IRelop(ValType.I32, OpCode.I32LtU)
  case object GtS extends IRelop(ValType.I32, OpCode.I32GtS)
  case object GtU extends IRelop(ValType.I32, OpCode.I32GtU)
  case object LeS extends IRelop(ValType.I32, OpCode.I32LeS)
  case object LeU extends IRelop(ValType.I32, OpCode.I32LeU)
  case object GeS extends IRelop(ValType.I32, OpCode.I32GeS)
  case object GeU extends IRelop(ValType.I32, OpCode.I32GeU)

  case object WrapI64 extends Convertop(ValType.I64, ValType.I32, OpCode.I32WrapI64)

  case object TruncSF32 extends Convertop(ValType.F32, ValType.I32, OpCode.I32TruncSF32)
  case object TruncUF32 extends Convertop(ValType.F32, ValType.I32, OpCode.I32TruncUF32)
  case object TruncSF64 extends Convertop(ValType.F64, ValType.I32, OpCode.I32TruncSF64)
  case object TruncUF64 extends Convertop(ValType.F64, ValType.I32, OpCode.I32TruncUF64)

  case object ReinterpretF32 extends Convertop(ValType.F32, ValType.I32, OpCode.I32ReinterpretF32)

  case class Load(offset: Int, align: Int) extends LoadInst(ValType.I32, OpCode.I32Load)
  case class Store(offset: Int, align: Int) extends StoreInst(ValType.I32, OpCode.I32Store)

  case class Load8S(offset: Int, align: Int) extends LoadNInst(ValType.I32, 8, OpCode.I32Load8S)
  case class Load8U(offset: Int, align: Int) extends LoadNInst(ValType.I32, 8, OpCode.I32Load8U)
  case class Load16S(offset: Int, align: Int) extends LoadNInst(ValType.I32, 16, OpCode.I32Load16S)
  case class Load16U(offset: Int, align: Int) extends LoadNInst(ValType.I32, 16, OpCode.I32Load16U)

  case class Store8(offset: Int, align: Int) extends StoreNInst(ValType.I32, 8, OpCode.I32Store8)
  case class Store16(offset: Int, align: Int) extends StoreNInst(ValType.I32, 16, OpCode.I32Store16)

}

object i64 {

  case class Const(v: Long) extends Inst(OpCode.I64Const)

  case object Clz extends IUnop(ValType.I64, OpCode.I64Clz)
  case object Ctz extends IUnop(ValType.I64, OpCode.I64Ctz)
  case object Popcnt extends IUnop(ValType.I64, OpCode.I64Popcnt)

  case object Add extends IBinop(ValType.I64, OpCode.I64Add)
  case object Sub extends IBinop(ValType.I64, OpCode.I64Sub)
  case object Mul extends IBinop(ValType.I64, OpCode.I64Mul)
  case object DivS extends IBinop(ValType.I64, OpCode.I64DivS)
  case object DivU extends IBinop(ValType.I64, OpCode.I64DivU)
  case object RemS extends IBinop(ValType.I64, OpCode.I64RemS)
  case object RemU extends IBinop(ValType.I64, OpCode.I64RemU)
  case object And extends IBinop(ValType.I64, OpCode.I64And)
  case object Or extends IBinop(ValType.I64, OpCode.I64Or)
  case object Xor extends IBinop(ValType.I64, OpCode.I64Xor)
  case object Shl extends IBinop(ValType.I64, OpCode.I64Shl)
  case object ShrS extends IBinop(ValType.I64, OpCode.I64ShrS)
  case object ShrU extends IBinop(ValType.I64, OpCode.I64ShrU)
  case object Rotl extends IBinop(ValType.I64, OpCode.I64Rotl)
  case object Rotr extends IBinop(ValType.I64, OpCode.I64Rotr)

  case object Eqz extends ITestop(ValType.I64, OpCode.I64Eqz)

  case object Eq extends IRelop(ValType.I64, OpCode.I64Eq)
  case object Ne extends IRelop(ValType.I64, OpCode.I64Ne)
  case object LtS extends IRelop(ValType.I64, OpCode.I64LtS)
  case object LtU extends IRelop(ValType.I64, OpCode.I64LtU)
  case object GtS extends IRelop(ValType.I64, OpCode.I64GtS)
  case object GtU extends IRelop(ValType.I64, OpCode.I64GtU)
  case object LeS extends IRelop(ValType.I64, OpCode.I64LeS)
  case object LeU extends IRelop(ValType.I64, OpCode.I64LeU)
  case object GeS extends IRelop(ValType.I64, OpCode.I64GeS)
  case object GeU extends IRelop(ValType.I64, OpCode.I64GeU)

  case object ExtendSI32 extends Convertop(ValType.I32, ValType.I64, OpCode.I64ExtendSI32)
  case object ExtendUI32 extends Convertop(ValType.I32, ValType.I64, OpCode.I64ExtendUI32)

  case object TruncSF32 extends Convertop(ValType.F32, ValType.I64, OpCode.I64TruncSF32)
  case object TruncUF32 extends Convertop(ValType.F32, ValType.I64, OpCode.I64TruncUF32)
  case object TruncSF64 extends Convertop(ValType.F64, ValType.I64, OpCode.I64TruncSF64)
  case object TruncUF64 extends Convertop(ValType.F64, ValType.I64, OpCode.I64TruncUF64)

  case object ReinterpretF64 extends Convertop(ValType.F64, ValType.I64, OpCode.I64ReinterpretF64)

  case class Load(offset: Int, align: Int) extends LoadInst(ValType.I64, OpCode.I64Load)
  case class Store(offset: Int, align: Int) extends StoreInst(ValType.I64, OpCode.I64Store)

  case class Load8S(offset: Int, align: Int) extends LoadNInst(ValType.I64, 8, OpCode.I64Load8S)
  case class Load8U(offset: Int, align: Int) extends LoadNInst(ValType.I64, 8, OpCode.I64Load8U)
  case class Load16S(offset: Int, align: Int) extends LoadNInst(ValType.I64, 16, OpCode.I64Load16S)
  case class Load16U(offset: Int, align: Int) extends LoadNInst(ValType.I64, 16, OpCode.I64Load16U)
  case class Load32S(offset: Int, align: Int) extends LoadNInst(ValType.I64, 32, OpCode.I64Load32S)
  case class Load32U(offset: Int, align: Int) extends LoadNInst(ValType.I64, 32, OpCode.I64Load32U)

  case class Store8(offset: Int, align: Int) extends StoreNInst(ValType.I64, 8, OpCode.I64Store8)
  case class Store16(offset: Int, align: Int) extends StoreNInst(ValType.I64, 16, OpCode.I64Store16)
  case class Store32(offset: Int, align: Int) extends StoreNInst(ValType.I64, 32, OpCode.I64Store32)

}

object f32 {

  case class Const(v: Float) extends Inst(OpCode.F32Const)

  case object Abs extends FUnop(ValType.F32, OpCode.F32Abs)
  case object Neg extends FUnop(ValType.F32, OpCode.F32Neg)
  case object Sqrt extends FUnop(ValType.F32, OpCode.F32Sqrt)
  case object Ceil extends FUnop(ValType.F32, OpCode.F32Ceil)
  case object Floor extends FUnop(ValType.F32, OpCode.F32Floor)
  case object Trunc extends FUnop(ValType.F32, OpCode.F32Trunc)
  case object Nearest extends FUnop(ValType.F32, OpCode.F32Nearest)

  case object Add extends FBinop(ValType.F32, OpCode.F32Add)
  case object Sub extends FBinop(ValType.F32, OpCode.F32Sub)
  case object Mul extends FBinop(ValType.F32, OpCode.F32Mul)
  case object Div extends FBinop(ValType.F32, OpCode.F32Div)
  case object Min extends FBinop(ValType.F32, OpCode.F32Min)
  case object Max extends FBinop(ValType.F32, OpCode.F32Max)
  case object Copysign extends FBinop(ValType.F32, OpCode.F32Copysign)

  case object Eq extends FRelop(ValType.F32, OpCode.F32Eq)
  case object Ne extends FRelop(ValType.F32, OpCode.F32Ne)
  case object Lt extends FRelop(ValType.F32, OpCode.F32Lt)
  case object Gt extends FRelop(ValType.F32, OpCode.F32Gt)
  case object Le extends FRelop(ValType.F32, OpCode.F32Le)
  case object Ge extends FRelop(ValType.F32, OpCode.F32Ge)

  case object DemoteF64 extends Convertop(ValType.F64, ValType.F32, OpCode.F32DemoteF64)

  case object ConvertSI32 extends Convertop(ValType.I32, ValType.F32, OpCode.F32ConvertSI32)
  case object ConvertUI32 extends Convertop(ValType.I32, ValType.F32, OpCode.F32ConvertUI32)
  case object ConvertSI64 extends Convertop(ValType.I64, ValType.F32, OpCode.F32ConvertSI64)
  case object ConvertUI64 extends Convertop(ValType.I64, ValType.F32, OpCode.F32ConvertUI64)

  case object ReinterpretI32 extends Convertop(ValType.I32, ValType.F32, OpCode.F32ReinterpretI32)

  case class Load(offset: Int, align: Int) extends LoadInst(ValType.F32, OpCode.F32Load)
  case class Store(offset: Int, align: Int) extends StoreInst(ValType.F32, OpCode.F32Store)

}

object f64 {

  case class Const(v: Double) extends Inst(OpCode.F64Const)

  case object Abs extends FUnop(ValType.F64, OpCode.F64Abs)
  case object Neg extends FUnop(ValType.F64, OpCode.F64Neg)
  case object Sqrt extends FUnop(ValType.F64, OpCode.F64Sqrt)
  case object Ceil extends FUnop(ValType.F64, OpCode.F64Ceil)
  case object Floor extends FUnop(ValType.F64, OpCode.F64Floor)
  case object Trunc extends FUnop(ValType.F64, OpCode.F64Trunc)
  case object Nearest extends FUnop(ValType.F64, OpCode.F64Nearest)

  case object Add extends FBinop(ValType.F64, OpCode.F64Add)
  case object Sub extends FBinop(ValType.F64, OpCode.F64Sub)
  case object Mul extends FBinop(ValType.F64, OpCode.F64Mul)
  case object Div extends FBinop(ValType.F64, OpCode.F64Div)
  case object Min extends FBinop(ValType.F64, OpCode.F64Min)
  case object Max extends FBinop(ValType.F64, OpCode.F64Max)
  case object Copysign extends FBinop(ValType.F64, OpCode.F64Copysign)

  case object Eq extends FRelop(ValType.F64, OpCode.F64Eq)
  case object Ne extends FRelop(ValType.F64, OpCode.F64Ne)
  case object Lt extends FRelop(ValType.F64, OpCode.F64Lt)
  case object Gt extends FRelop(ValType.F64, OpCode.F64Gt)
  case object Le extends FRelop(ValType.F64, OpCode.F64Le)
  case object Ge extends FRelop(ValType.F64, OpCode.F64Ge)

  case object PromoteF32 extends Convertop(ValType.F32, ValType.F64, OpCode.F64PromoteF32)

  case object ConvertSI32 extends Convertop(ValType.I32, ValType.F64, OpCode.F64ConvertSI32)
  case object ConvertUI32 extends Convertop(ValType.I32, ValType.F64, OpCode.F64ConvertUI32)
  case object ConvertSI64 extends Convertop(ValType.I64, ValType.F64, OpCode.F64ConvertSI64)
  case object ConvertUI64 extends Convertop(ValType.I64, ValType.F64, OpCode.F64ConvertUI64)

  case object ReinterpretI64 extends Convertop(ValType.I64, ValType.F64, OpCode.F64ReinterpretI64)

  case class Load(offset: Int, align: Int) extends LoadInst(ValType.F64, OpCode.F64Load)
  case class Store(offset: Int, align: Int) extends StoreInst(ValType.F64, OpCode.F64Store)

}

case object Drop extends Inst(OpCode.Drop)
case object Select extends Inst(OpCode.Select)

case class GetLocal(idx: LocalIdx) extends VarInst(OpCode.GetLocal)
case class SetLocal(idx: LocalIdx) extends VarInst(OpCode.SetLocal)
case class TeeLocal(idx: LocalIdx) extends VarInst(OpCode.TeeLocal)
case class GetGlobal(idx: LocalIdx) extends VarInst(OpCode.GetGlobal)
case class SetGlobal(idx: LocalIdx) extends VarInst(OpCode.SetGlobal)

case object MemorySize extends Inst(OpCode.MemorySize)
case object MemoryGrow extends Inst(OpCode.MemoryGrow)

case object Nop extends Inst(OpCode.Nop)
case object Unreachable extends Inst(OpCode.Unreachable)
case class Block(tpe: ResultType, instr: Vector[Inst]) extends Inst(OpCode.Block)
case class Loop(tpe: ResultType, instr: Vector[Inst]) extends Inst(OpCode.Loop)
case class If(tpe: ResultType, thenInstr: Vector[Inst], elseInstr: Vector[Inst]) extends Inst(OpCode.If)
case class Br(lbl: LabelIdx) extends Inst(OpCode.Br)
case class BrIf(lbl: LabelIdx) extends Inst(OpCode.BrIf)
case class BrTable(table: Vector[LabelIdx], lbl: LabelIdx) extends Inst(OpCode.BrTable)
case object Return extends Inst(OpCode.Return)
case class Call(funcidx: FuncIdx) extends Inst(OpCode.Call)
case class CallIndirect(typeidx: TypeIdx) extends Inst(OpCode.CallIndirect)
