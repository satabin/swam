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

sealed abstract class IUnop(opcode: OpCode) extends Inst(opcode)

sealed abstract class IBinop(opcode: OpCode) extends Inst(opcode)

sealed abstract class ITestop(opcode: OpCode) extends Inst(opcode)

sealed abstract class IRelop(opcode: OpCode) extends Inst(opcode)

sealed abstract class FUnop(opcode: OpCode) extends Inst(opcode)

sealed abstract class FBinop(opcode: OpCode) extends Inst(opcode)

sealed abstract class FRelop(opcode: OpCode) extends Inst(opcode)

sealed abstract class MemoryInst(opcode: OpCode) extends Inst(opcode) {
  val offset: Int
  val align: Int
}

object MemoryInst {
  def unapply(inst: MemoryInst): Option[(Int, Int)] =
    Some(inst.offset -> inst.align)
}

object i32 {

  case class Const(v: Int) extends Inst(OpCode.I32Const)

  case object Clz extends IUnop(OpCode.I32Clz)
  case object Ctz extends IUnop(OpCode.I32Ctz)
  case object Popcnt extends IUnop(OpCode.I32Popcnt)

  case object Add extends IBinop(OpCode.I32Add)
  case object Sub extends IBinop(OpCode.I32Sub)
  case object Mul extends IBinop(OpCode.I32Mul)
  case object DivS extends IBinop(OpCode.I32DivS)
  case object DivU extends IBinop(OpCode.I32DivU)
  case object RemS extends IBinop(OpCode.I32RemS)
  case object RemU extends IBinop(OpCode.I32RemU)
  case object And extends IBinop(OpCode.I32And)
  case object Or extends IBinop(OpCode.I32Or)
  case object Xor extends IBinop(OpCode.I32Xor)
  case object Shl extends IBinop(OpCode.I32Shl)
  case object ShrS extends IBinop(OpCode.I32ShrS)
  case object ShrU extends IBinop(OpCode.I32ShrU)
  case object Rotl extends IBinop(OpCode.I32Rotl)
  case object Rotr extends IBinop(OpCode.I32Rotr)

  case object Eqz extends ITestop(OpCode.I32Eqz)

  case object Eq extends IRelop(OpCode.I32Eq)
  case object Ne extends IRelop(OpCode.I32Ne)
  case object LtS extends IRelop(OpCode.I32LtS)
  case object LtU extends IRelop(OpCode.I32LtU)
  case object GtS extends IRelop(OpCode.I32GtS)
  case object GtU extends IRelop(OpCode.I32GtU)
  case object LeS extends IRelop(OpCode.I32LeS)
  case object LeU extends IRelop(OpCode.I32LeU)
  case object GeS extends IRelop(OpCode.I32GeS)
  case object GeU extends IRelop(OpCode.I32GeU)

  case object WrapI64 extends Inst(OpCode.I32WrapI64)

  case object TruncSF32 extends Inst(OpCode.I32TruncSF32)
  case object TruncUF32 extends Inst(OpCode.I32TruncUF32)
  case object TruncSF64 extends Inst(OpCode.I32TruncSF64)
  case object TruncUF64 extends Inst(OpCode.I32TruncUF64)

  case object ReinterpretF32 extends Inst(OpCode.I32ReinterpretF32)

  case class Load(offset: Int, align: Int) extends MemoryInst(OpCode.I32Load)
  case class Store(offset: Int, align: Int) extends MemoryInst(OpCode.I32Store)

  case class Load8S(offset: Int, align: Int) extends MemoryInst(OpCode.I32Load8S)
  case class Load8U(offset: Int, align: Int) extends MemoryInst(OpCode.I32Load8U)
  case class Load16S(offset: Int, align: Int) extends MemoryInst(OpCode.I32Load16S)
  case class Load16U(offset: Int, align: Int) extends MemoryInst(OpCode.I32Load16U)

  case class Store8(offset: Int, align: Int) extends MemoryInst(OpCode.I32Store8)
  case class Store16(offset: Int, align: Int) extends MemoryInst(OpCode.I32Store16)

}

object i64 {

  case class Const(v: Long) extends Inst(OpCode.I64Const)

  case object Clz extends IUnop(OpCode.I64Clz)
  case object Ctz extends IUnop(OpCode.I64Ctz)
  case object Popcnt extends IUnop(OpCode.I64Popcnt)

  case object Add extends IBinop(OpCode.I64Add)
  case object Sub extends IBinop(OpCode.I64Sub)
  case object Mul extends IBinop(OpCode.I64Mul)
  case object DivS extends IBinop(OpCode.I64DivS)
  case object DivU extends IBinop(OpCode.I64DivU)
  case object RemS extends IBinop(OpCode.I64RemS)
  case object RemU extends IBinop(OpCode.I64RemU)
  case object And extends IBinop(OpCode.I64And)
  case object Or extends IBinop(OpCode.I64Or)
  case object Xor extends IBinop(OpCode.I64Xor)
  case object Shl extends IBinop(OpCode.I64Shl)
  case object ShrS extends IBinop(OpCode.I64ShrS)
  case object ShrU extends IBinop(OpCode.I64ShrU)
  case object Rotl extends IBinop(OpCode.I64Rotl)
  case object Rotr extends IBinop(OpCode.I64Rotr)

  case object Eqz extends ITestop(OpCode.I64Eqz)

  case object Eq extends IRelop(OpCode.I64Eq)
  case object Ne extends IRelop(OpCode.I64Ne)
  case object LtS extends IRelop(OpCode.I64LtS)
  case object LtU extends IRelop(OpCode.I64LtU)
  case object GtS extends IRelop(OpCode.I64GtS)
  case object GtU extends IRelop(OpCode.I64GtU)
  case object LeS extends IRelop(OpCode.I64LeS)
  case object LeU extends IRelop(OpCode.I64LeU)
  case object GeS extends IRelop(OpCode.I64GeS)
  case object GeU extends IRelop(OpCode.I64GeU)

  case object ExtendSI32 extends Inst(OpCode.I64ExtendSI32)
  case object ExtendUI32 extends Inst(OpCode.I64ExtendUI32)

  case object TruncSF32 extends Inst(OpCode.I64TruncSF32)
  case object TruncUF32 extends Inst(OpCode.I64TruncUF32)
  case object TruncSF64 extends Inst(OpCode.I64TruncSF64)
  case object TruncUF64 extends Inst(OpCode.I64TruncUF64)

  case object ReinterpretF64 extends Inst(OpCode.I64ReinterpretF64)

  case class Load(offset: Int, align: Int) extends MemoryInst(OpCode.I64Load)
  case class Store(offset: Int, align: Int) extends MemoryInst(OpCode.I64Store)

  case class Load8S(offset: Int, align: Int) extends MemoryInst(OpCode.I64Load8S)
  case class Load8U(offset: Int, align: Int) extends MemoryInst(OpCode.I64Load8U)
  case class Load16S(offset: Int, align: Int) extends MemoryInst(OpCode.I64Load16S)
  case class Load16U(offset: Int, align: Int) extends MemoryInst(OpCode.I64Load16U)
  case class Load32S(offset: Int, align: Int) extends MemoryInst(OpCode.I64Load32S)
  case class Load32U(offset: Int, align: Int) extends MemoryInst(OpCode.I64Load32U)

  case class Store8(offset: Int, align: Int) extends MemoryInst(OpCode.I64Store8)
  case class Store16(offset: Int, align: Int) extends MemoryInst(OpCode.I64Store16)
  case class Store32(offset: Int, align: Int) extends MemoryInst(OpCode.I64Store32)

}

object f32 {

  case class Const(v: Float) extends Inst(OpCode.F32Const)

  case object Abs extends FUnop(OpCode.F32Abs)
  case object Neg extends FUnop(OpCode.F32Neg)
  case object Sqrt extends FUnop(OpCode.F32Sqrt)
  case object Ceil extends FUnop(OpCode.F32Ceil)
  case object Floor extends FUnop(OpCode.F32Floor)
  case object Trunc extends FUnop(OpCode.F32Trunc)
  case object Nearest extends FUnop(OpCode.F32Nearest)

  case object Add extends FBinop(OpCode.F32Add)
  case object Sub extends FBinop(OpCode.F32Sub)
  case object Mul extends FBinop(OpCode.F32Mul)
  case object Div extends FBinop(OpCode.F32Div)
  case object Min extends FBinop(OpCode.F32Min)
  case object Max extends FBinop(OpCode.F32Max)
  case object Copysign extends FBinop(OpCode.F32Copysign)

  case object Eq extends FRelop(OpCode.F32Eq)
  case object Ne extends FRelop(OpCode.F32Ne)
  case object Lt extends FRelop(OpCode.F32Lt)
  case object Gt extends FRelop(OpCode.F32Gt)
  case object Le extends FRelop(OpCode.F32Le)
  case object Ge extends FRelop(OpCode.F32Ge)

  case object DemoteF64 extends Inst(OpCode.F32DemoteF64)

  case object ConvertSI32 extends Inst(OpCode.F32ConvertSI32)
  case object ConvertUI32 extends Inst(OpCode.F32ConvertUI32)
  case object ConvertSI64 extends Inst(OpCode.F32ConvertSI64)
  case object ConvertUI64 extends Inst(OpCode.F32ConvertUI64)

  case object ReinterpretI32 extends Inst(OpCode.F32ReinterpretI32)

  case class Load(offset: Int, align: Int) extends MemoryInst(OpCode.F32Load)
  case class Store(offset: Int, align: Int) extends MemoryInst(OpCode.F32Store)

}

object f64 {

  case class Const(v: Double) extends Inst(OpCode.F64Const)

  case object Abs extends FUnop(OpCode.F64Abs)
  case object Neg extends FUnop(OpCode.F64Neg)
  case object Sqrt extends FUnop(OpCode.F64Sqrt)
  case object Ceil extends FUnop(OpCode.F64Ceil)
  case object Floor extends FUnop(OpCode.F64Floor)
  case object Trunc extends FUnop(OpCode.F64Trunc)
  case object Nearest extends FUnop(OpCode.F64Nearest)

  case object Add extends FBinop(OpCode.F64Add)
  case object Sub extends FBinop(OpCode.F64Sub)
  case object Mul extends FBinop(OpCode.F64Mul)
  case object Div extends FBinop(OpCode.F64Div)
  case object Min extends FBinop(OpCode.F64Min)
  case object Max extends FBinop(OpCode.F64Max)
  case object Copysign extends FBinop(OpCode.F64Copysign)

  case object Eq extends FRelop(OpCode.F64Eq)
  case object Ne extends FRelop(OpCode.F64Ne)
  case object Lt extends FRelop(OpCode.F64Lt)
  case object Gt extends FRelop(OpCode.F64Gt)
  case object Le extends FRelop(OpCode.F64Le)
  case object Ge extends FRelop(OpCode.F64Ge)

  case object PromoteF32 extends Inst(OpCode.F64PromoteF32)

  case object ConvertSI32 extends Inst(OpCode.F64ConvertSI32)
  case object ConvertUI32 extends Inst(OpCode.F64ConvertUI32)
  case object ConvertSI64 extends Inst(OpCode.F64ConvertSI64)
  case object ConvertUI64 extends Inst(OpCode.F64ConvertUI64)

  case object ReinterpretI64 extends Inst(OpCode.F64ReinterpretI64)

  case class Load(offset: Int, align: Int) extends MemoryInst(OpCode.F64Load)
  case class Store(offset: Int, align: Int) extends MemoryInst(OpCode.F64Store)

}

case object Drop extends Inst(OpCode.Drop)
case object Select extends Inst(OpCode.Select)

case class GetLocal(idx: LocalIdx) extends Inst(OpCode.GetLocal)
case class SetLocal(idx: LocalIdx) extends Inst(OpCode.SetLocal)
case class TeeLocal(idx: LocalIdx) extends Inst(OpCode.TeeLocal)
case class GetGlobal(idx: LocalIdx) extends Inst(OpCode.GetGlobal)
case class SetGlobal(idx: LocalIdx) extends Inst(OpCode.SetGlobal)

case object CurrentMemory extends Inst(OpCode.CurrentMemory)
case object GrowMemory extends Inst(OpCode.GrowMemory)

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
case class CallIndirect(typeidc: TypeIdx) extends Inst(OpCode.CallIndirect)
