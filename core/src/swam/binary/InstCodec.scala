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
import scodec.bits._
import scodec.codecs._
import scodec.codecs.literals._

import scala.annotation.switch

trait InstCodec extends TypeCodec {

  private val end: Codec[Unit] =
    hex"0b"

  private val block: Codec[(BlockType, Vector[Inst])] =
    blockType ~ lazily(expr)

  private val ifThenElse: Codec[BlockType ~ Vector[Inst] ~ Vector[Inst]] =
    "if" | lazily(
      ("return type" | blockType) ~ ("then" | instructions) ~ withDefaultValue(
        optional(recover(hex"05"), "else" | instructions),
        Vector.empty
      ) <~ end
    )

  private val brtable: Codec[Vector[LabelIdx] ~ LabelIdx] =
    vectorOfN(varuint32, varuint32) ~ varuint32

  private val callindirect: Codec[TypeIdx] =
    varuint32 <~ hex"00"

  private val memarg: Codec[(Int, Int)] =
    varuint32 ~ varuint32

  private val misc: Codec[Miscop] =
    mappedEnum(
      byte,
      Map[Miscop, Byte](
        i32.TruncSatSF32 -> 0x00,
        i32.TruncSatUF32 -> 0x01,
        i32.TruncSatSF64 -> 0x02,
        i32.TruncSatUF64 -> 0x03,
        i64.TruncSatSF32 -> 0x04,
        i64.TruncSatUF32 -> 0x05,
        i64.TruncSatSF64 -> 0x06,
        i64.TruncSatUF64 -> 0x07
      )
    )

  val opcode: Codec[OpCode] = new Codec[OpCode] {
    def decode(bits: BitVector): Attempt[DecodeResult[OpCode]] =
      if (bits.size < 8) {
        Attempt.failure(Err("at least 8 bits are expected"))
      } else {
        val (head, tail) = bits.splitAt(8)
        val byte = head.toByte()
        OpCode.withValueOpt(byte & 0xff) match {
          case Some(opcode) => Attempt.successful(DecodeResult(opcode, tail))
          case None         => Attempt.failure(Err(f"Unknown opcode 0x$byte%02x"))
        }
      }
    def encode(opcode: OpCode): Attempt[BitVector] =
      Attempt.successful(BitVector.fromByte(opcode.toByte))
    def sizeBound: SizeBound = SizeBound.exact(8L)

  }

  val instruction: Codec[Inst] = new Codec[Inst] {

    def decode(bits: BitVector): Attempt[DecodeResult[Inst]] =
      opcode.decode(bits).flatMap {
        case DecodeResult(opcode, remainder) =>
          (opcode: @switch) match {
            case OpCode.Unreachable =>
              Attempt.successful(DecodeResult(Unreachable, remainder))
            case OpCode.Nop     => Attempt.successful(DecodeResult(Nop, remainder))
            case OpCode.Block   => block.decode(remainder).map(_.map(Block))
            case OpCode.Loop    => block.decode(remainder).map(_.map(Loop))
            case OpCode.If      => ifThenElse.decode(remainder).map(_.map(If))
            case OpCode.Br      => varuint32.decode(remainder).map(_.map(Br))
            case OpCode.BrIf    => varuint32.decode(remainder).map(_.map(BrIf))
            case OpCode.BrTable => brtable.decode(remainder).map(_.map(BrTable))
            case OpCode.Return =>
              Attempt.successful(DecodeResult(Return, remainder))
            case OpCode.Call => varuint32.decode(remainder).map(_.map(Call))
            case OpCode.CallIndirect =>
              callindirect.decode(remainder).map(_.map(CallIndirect))
            case OpCode.Drop =>
              Attempt.successful(DecodeResult(Drop, remainder))
            case OpCode.Select =>
              Attempt.successful(DecodeResult(Select, remainder))
            case OpCode.LocalGet =>
              varuint32.decode(remainder).map(_.map(LocalGet))
            case OpCode.LocalSet =>
              varuint32.decode(remainder).map(_.map(LocalSet))
            case OpCode.LocalTee =>
              varuint32.decode(remainder).map(_.map(LocalTee))
            case OpCode.GlobalGet =>
              varuint32.decode(remainder).map(_.map(GlobalGet))
            case OpCode.GlobalSet =>
              varuint32.decode(remainder).map(_.map(GlobalSet))
            case OpCode.I32Load => memarg.decode(remainder).map(_.map(i32.Load))
            case OpCode.I64Load => memarg.decode(remainder).map(_.map(i64.Load))
            case OpCode.F32Load => memarg.decode(remainder).map(_.map(f32.Load))
            case OpCode.F64Load => memarg.decode(remainder).map(_.map(f64.Load))
            case OpCode.I32Load8S =>
              memarg.decode(remainder).map(_.map(i32.Load8S))
            case OpCode.I32Load8U =>
              memarg.decode(remainder).map(_.map(i32.Load8U))
            case OpCode.I32Load16S =>
              memarg.decode(remainder).map(_.map(i32.Load16S))
            case OpCode.I32Load16U =>
              memarg.decode(remainder).map(_.map(i32.Load16U))
            case OpCode.I64Load8S =>
              memarg.decode(remainder).map(_.map(i64.Load8S))
            case OpCode.I64Load8U =>
              memarg.decode(remainder).map(_.map(i64.Load8U))
            case OpCode.I64Load16S =>
              memarg.decode(remainder).map(_.map(i64.Load16S))
            case OpCode.I64Load16U =>
              memarg.decode(remainder).map(_.map(i64.Load16U))
            case OpCode.I64Load32S =>
              memarg.decode(remainder).map(_.map(i64.Load32S))
            case OpCode.I64Load32U =>
              memarg.decode(remainder).map(_.map(i64.Load32U))
            case OpCode.I32Store =>
              memarg.decode(remainder).map(_.map(i32.Store))
            case OpCode.I64Store =>
              memarg.decode(remainder).map(_.map(i64.Store))
            case OpCode.F32Store =>
              memarg.decode(remainder).map(_.map(f32.Store))
            case OpCode.F64Store =>
              memarg.decode(remainder).map(_.map(f64.Store))
            case OpCode.I32Store8 =>
              memarg.decode(remainder).map(_.map(i32.Store8))
            case OpCode.I32Store16 =>
              memarg.decode(remainder).map(_.map(i32.Store16))
            case OpCode.I64Store8 =>
              memarg.decode(remainder).map(_.map(i64.Store8))
            case OpCode.I64Store16 =>
              memarg.decode(remainder).map(_.map(i64.Store16))
            case OpCode.I64Store32 =>
              memarg.decode(remainder).map(_.map(i64.Store32))
            case OpCode.MemorySize =>
              constant(hex"00").decode(remainder).map(_.map(_ => MemorySize))
            case OpCode.MemoryGrow =>
              constant(hex"00").decode(remainder).map(_.map(_ => MemoryGrow))
            case OpCode.I32Const =>
              varint32.decode(remainder).map(_.map(i32.Const))
            case OpCode.I64Const =>
              varint64.decode(remainder).map(_.map(i64.Const))
            case OpCode.F32Const =>
              floatL.decode(remainder).map(_.map(f32.Const))
            case OpCode.F64Const =>
              doubleL.decode(remainder).map(_.map(f64.Const))
            case OpCode.I32Eqz =>
              Attempt.successful(DecodeResult(i32.Eqz, remainder))
            case OpCode.I32Eq =>
              Attempt.successful(DecodeResult(i32.Eq, remainder))
            case OpCode.I32Ne =>
              Attempt.successful(DecodeResult(i32.Ne, remainder))
            case OpCode.I32LtS =>
              Attempt.successful(DecodeResult(i32.LtS, remainder))
            case OpCode.I32LtU =>
              Attempt.successful(DecodeResult(i32.LtU, remainder))
            case OpCode.I32GtS =>
              Attempt.successful(DecodeResult(i32.GtS, remainder))
            case OpCode.I32GtU =>
              Attempt.successful(DecodeResult(i32.GtU, remainder))
            case OpCode.I32LeS =>
              Attempt.successful(DecodeResult(i32.LeS, remainder))
            case OpCode.I32LeU =>
              Attempt.successful(DecodeResult(i32.LeU, remainder))
            case OpCode.I32GeS =>
              Attempt.successful(DecodeResult(i32.GeS, remainder))
            case OpCode.I32GeU =>
              Attempt.successful(DecodeResult(i32.GeU, remainder))
            case OpCode.I64Eqz =>
              Attempt.successful(DecodeResult(i64.Eqz, remainder))
            case OpCode.I64Eq =>
              Attempt.successful(DecodeResult(i64.Eq, remainder))
            case OpCode.I64Ne =>
              Attempt.successful(DecodeResult(i64.Ne, remainder))
            case OpCode.I64LtS =>
              Attempt.successful(DecodeResult(i64.LtS, remainder))
            case OpCode.I64LtU =>
              Attempt.successful(DecodeResult(i64.LtU, remainder))
            case OpCode.I64GtS =>
              Attempt.successful(DecodeResult(i64.GtS, remainder))
            case OpCode.I64GtU =>
              Attempt.successful(DecodeResult(i64.GtU, remainder))
            case OpCode.I64LeS =>
              Attempt.successful(DecodeResult(i64.LeS, remainder))
            case OpCode.I64LeU =>
              Attempt.successful(DecodeResult(i64.LeU, remainder))
            case OpCode.I64GeS =>
              Attempt.successful(DecodeResult(i64.GeS, remainder))
            case OpCode.I64GeU =>
              Attempt.successful(DecodeResult(i64.GeU, remainder))
            case OpCode.F32Eq =>
              Attempt.successful(DecodeResult(f32.Eq, remainder))
            case OpCode.F32Ne =>
              Attempt.successful(DecodeResult(f32.Ne, remainder))
            case OpCode.F32Lt =>
              Attempt.successful(DecodeResult(f32.Lt, remainder))
            case OpCode.F32Gt =>
              Attempt.successful(DecodeResult(f32.Gt, remainder))
            case OpCode.F32Le =>
              Attempt.successful(DecodeResult(f32.Le, remainder))
            case OpCode.F32Ge =>
              Attempt.successful(DecodeResult(f32.Ge, remainder))
            case OpCode.F64Eq =>
              Attempt.successful(DecodeResult(f64.Eq, remainder))
            case OpCode.F64Ne =>
              Attempt.successful(DecodeResult(f64.Ne, remainder))
            case OpCode.F64Lt =>
              Attempt.successful(DecodeResult(f64.Lt, remainder))
            case OpCode.F64Gt =>
              Attempt.successful(DecodeResult(f64.Gt, remainder))
            case OpCode.F64Le =>
              Attempt.successful(DecodeResult(f64.Le, remainder))
            case OpCode.F64Ge =>
              Attempt.successful(DecodeResult(f64.Ge, remainder))
            case OpCode.I32Clz =>
              Attempt.successful(DecodeResult(i32.Clz, remainder))
            case OpCode.I32Ctz =>
              Attempt.successful(DecodeResult(i32.Ctz, remainder))
            case OpCode.I32Popcnt =>
              Attempt.successful(DecodeResult(i32.Popcnt, remainder))
            case OpCode.I32Add =>
              Attempt.successful(DecodeResult(i32.Add, remainder))
            case OpCode.I32Sub =>
              Attempt.successful(DecodeResult(i32.Sub, remainder))
            case OpCode.I32Mul =>
              Attempt.successful(DecodeResult(i32.Mul, remainder))
            case OpCode.I32DivS =>
              Attempt.successful(DecodeResult(i32.DivS, remainder))
            case OpCode.I32DivU =>
              Attempt.successful(DecodeResult(i32.DivU, remainder))
            case OpCode.I32RemS =>
              Attempt.successful(DecodeResult(i32.RemS, remainder))
            case OpCode.I32RemU =>
              Attempt.successful(DecodeResult(i32.RemU, remainder))
            case OpCode.I32And =>
              Attempt.successful(DecodeResult(i32.And, remainder))
            case OpCode.I32Or =>
              Attempt.successful(DecodeResult(i32.Or, remainder))
            case OpCode.I32Xor =>
              Attempt.successful(DecodeResult(i32.Xor, remainder))
            case OpCode.I32Shl =>
              Attempt.successful(DecodeResult(i32.Shl, remainder))
            case OpCode.I32ShrS =>
              Attempt.successful(DecodeResult(i32.ShrS, remainder))
            case OpCode.I32ShrU =>
              Attempt.successful(DecodeResult(i32.ShrU, remainder))
            case OpCode.I32Rotl =>
              Attempt.successful(DecodeResult(i32.Rotl, remainder))
            case OpCode.I32Rotr =>
              Attempt.successful(DecodeResult(i32.Rotr, remainder))
            case OpCode.I64Clz =>
              Attempt.successful(DecodeResult(i64.Clz, remainder))
            case OpCode.I64Ctz =>
              Attempt.successful(DecodeResult(i64.Ctz, remainder))
            case OpCode.I64Popcnt =>
              Attempt.successful(DecodeResult(i64.Popcnt, remainder))
            case OpCode.I64Add =>
              Attempt.successful(DecodeResult(i64.Add, remainder))
            case OpCode.I64Sub =>
              Attempt.successful(DecodeResult(i64.Sub, remainder))
            case OpCode.I64Mul =>
              Attempt.successful(DecodeResult(i64.Mul, remainder))
            case OpCode.I64DivS =>
              Attempt.successful(DecodeResult(i64.DivS, remainder))
            case OpCode.I64DivU =>
              Attempt.successful(DecodeResult(i64.DivU, remainder))
            case OpCode.I64RemS =>
              Attempt.successful(DecodeResult(i64.RemS, remainder))
            case OpCode.I64RemU =>
              Attempt.successful(DecodeResult(i64.RemU, remainder))
            case OpCode.I64And =>
              Attempt.successful(DecodeResult(i64.And, remainder))
            case OpCode.I64Or =>
              Attempt.successful(DecodeResult(i64.Or, remainder))
            case OpCode.I64Xor =>
              Attempt.successful(DecodeResult(i64.Xor, remainder))
            case OpCode.I64Shl =>
              Attempt.successful(DecodeResult(i64.Shl, remainder))
            case OpCode.I64ShrS =>
              Attempt.successful(DecodeResult(i64.ShrS, remainder))
            case OpCode.I64ShrU =>
              Attempt.successful(DecodeResult(i64.ShrU, remainder))
            case OpCode.I64Rotl =>
              Attempt.successful(DecodeResult(i64.Rotl, remainder))
            case OpCode.I64Rotr =>
              Attempt.successful(DecodeResult(i64.Rotr, remainder))
            case OpCode.F32Abs =>
              Attempt.successful(DecodeResult(f32.Abs, remainder))
            case OpCode.F32Neg =>
              Attempt.successful(DecodeResult(f32.Neg, remainder))
            case OpCode.F32Ceil =>
              Attempt.successful(DecodeResult(f32.Ceil, remainder))
            case OpCode.F32Floor =>
              Attempt.successful(DecodeResult(f32.Floor, remainder))
            case OpCode.F32Trunc =>
              Attempt.successful(DecodeResult(f32.Trunc, remainder))
            case OpCode.F32Nearest =>
              Attempt.successful(DecodeResult(f32.Nearest, remainder))
            case OpCode.F32Sqrt =>
              Attempt.successful(DecodeResult(f32.Sqrt, remainder))
            case OpCode.F32Add =>
              Attempt.successful(DecodeResult(f32.Add, remainder))
            case OpCode.F32Sub =>
              Attempt.successful(DecodeResult(f32.Sub, remainder))
            case OpCode.F32Mul =>
              Attempt.successful(DecodeResult(f32.Mul, remainder))
            case OpCode.F32Div =>
              Attempt.successful(DecodeResult(f32.Div, remainder))
            case OpCode.F32Min =>
              Attempt.successful(DecodeResult(f32.Min, remainder))
            case OpCode.F32Max =>
              Attempt.successful(DecodeResult(f32.Max, remainder))
            case OpCode.F32Copysign =>
              Attempt.successful(DecodeResult(f32.Copysign, remainder))
            case OpCode.F64Abs =>
              Attempt.successful(DecodeResult(f64.Abs, remainder))
            case OpCode.F64Neg =>
              Attempt.successful(DecodeResult(f64.Neg, remainder))
            case OpCode.F64Ceil =>
              Attempt.successful(DecodeResult(f64.Ceil, remainder))
            case OpCode.F64Floor =>
              Attempt.successful(DecodeResult(f64.Floor, remainder))
            case OpCode.F64Trunc =>
              Attempt.successful(DecodeResult(f64.Trunc, remainder))
            case OpCode.F64Nearest =>
              Attempt.successful(DecodeResult(f64.Nearest, remainder))
            case OpCode.F64Sqrt =>
              Attempt.successful(DecodeResult(f64.Sqrt, remainder))
            case OpCode.F64Add =>
              Attempt.successful(DecodeResult(f64.Add, remainder))
            case OpCode.F64Sub =>
              Attempt.successful(DecodeResult(f64.Sub, remainder))
            case OpCode.F64Mul =>
              Attempt.successful(DecodeResult(f64.Mul, remainder))
            case OpCode.F64Div =>
              Attempt.successful(DecodeResult(f64.Div, remainder))
            case OpCode.F64Min =>
              Attempt.successful(DecodeResult(f64.Min, remainder))
            case OpCode.F64Max =>
              Attempt.successful(DecodeResult(f64.Max, remainder))
            case OpCode.F64Copysign =>
              Attempt.successful(DecodeResult(f64.Copysign, remainder))
            case OpCode.I32WrapI64 =>
              Attempt.successful(DecodeResult(i32.WrapI64, remainder))
            case OpCode.I32TruncSF32 =>
              Attempt.successful(DecodeResult(i32.TruncSF32, remainder))
            case OpCode.I32TruncUF32 =>
              Attempt.successful(DecodeResult(i32.TruncUF32, remainder))
            case OpCode.I32TruncSF64 =>
              Attempt.successful(DecodeResult(i32.TruncSF64, remainder))
            case OpCode.I32TruncUF64 =>
              Attempt.successful(DecodeResult(i32.TruncUF64, remainder))
            case OpCode.I64ExtendSI32 =>
              Attempt.successful(DecodeResult(i64.ExtendSI32, remainder))
            case OpCode.I64ExtendUI32 =>
              Attempt.successful(DecodeResult(i64.ExtendUI32, remainder))
            case OpCode.I64TruncSF32 =>
              Attempt.successful(DecodeResult(i64.TruncSF32, remainder))
            case OpCode.I64TruncUF32 =>
              Attempt.successful(DecodeResult(i64.TruncUF32, remainder))
            case OpCode.I64TruncSF64 =>
              Attempt.successful(DecodeResult(i64.TruncSF64, remainder))
            case OpCode.I64TruncUF64 =>
              Attempt.successful(DecodeResult(i64.TruncUF64, remainder))
            case OpCode.F32ConvertSI32 =>
              Attempt.successful(DecodeResult(f32.ConvertSI32, remainder))
            case OpCode.F32ConvertUI32 =>
              Attempt.successful(DecodeResult(f32.ConvertUI32, remainder))
            case OpCode.F32ConvertSI64 =>
              Attempt.successful(DecodeResult(f32.ConvertSI64, remainder))
            case OpCode.F32ConvertUI64 =>
              Attempt.successful(DecodeResult(f32.ConvertUI64, remainder))
            case OpCode.F32DemoteF64 =>
              Attempt.successful(DecodeResult(f32.DemoteF64, remainder))
            case OpCode.F64ConvertSI32 =>
              Attempt.successful(DecodeResult(f64.ConvertSI32, remainder))
            case OpCode.F64ConvertUI32 =>
              Attempt.successful(DecodeResult(f64.ConvertUI32, remainder))
            case OpCode.F64ConvertSI64 =>
              Attempt.successful(DecodeResult(f64.ConvertSI64, remainder))
            case OpCode.F64ConvertUI64 =>
              Attempt.successful(DecodeResult(f64.ConvertUI64, remainder))
            case OpCode.F64PromoteF32 =>
              Attempt.successful(DecodeResult(f64.PromoteF32, remainder))
            case OpCode.I32ReinterpretF32 =>
              Attempt.successful(DecodeResult(i32.ReinterpretF32, remainder))
            case OpCode.I64ReinterpretF64 =>
              Attempt.successful(DecodeResult(i64.ReinterpretF64, remainder))
            case OpCode.F32ReinterpretI32 =>
              Attempt.successful(DecodeResult(f32.ReinterpretI32, remainder))
            case OpCode.F64ReinterpretI64 =>
              Attempt.successful(DecodeResult(f64.ReinterpretI64, remainder))
            case OpCode.I32Extend8S =>
              Attempt.successful(DecodeResult(i32.Extend8S, remainder))
            case OpCode.I32Extend16S =>
              Attempt.successful(DecodeResult(i32.Extend16S, remainder))
            case OpCode.I64Extend8S =>
              Attempt.successful(DecodeResult(i64.Extend8S, remainder))
            case OpCode.I64Extend16S =>
              Attempt.successful(DecodeResult(i64.Extend16S, remainder))
            case OpCode.I64Extend32S =>
              Attempt.successful(DecodeResult(i64.Extend32S, remainder))
            case OpCode.MiscOp => misc.decode(remainder)
            case _ =>
              Attempt.Failure(Err(f"Unknown opcode 0x$opcode%02x"))
          }
      }

    def encode(inst: Inst): Attempt[BitVector] =
      inst match {
        case Unreachable =>
          Attempt.successful(BitVector.fromByte(OpCode.Unreachable.toByte))
        case Nop => Attempt.successful(BitVector.fromByte(OpCode.Nop.toByte))
        case Block(tpe, insts) =>
          block
            .encode(tpe ~ insts)
            .map(BitVector.fromByte(OpCode.Block.toByte) ++ _)
        case Loop(tpe, insts) =>
          block
            .encode(tpe ~ insts)
            .map(BitVector.fromByte(OpCode.Loop.toByte) ++ _)
        case If(tpe, thenInst, elseInst) =>
          ifThenElse
            .encode(tpe ~ thenInst ~ elseInst)
            .map(BitVector.fromByte(OpCode.If.toByte) ++ _)
        case Br(lbl) =>
          varuint32.encode(lbl).map(BitVector.fromByte(OpCode.Br.toByte) ++ _)
        case BrIf(lbl) =>
          varuint32.encode(lbl).map(BitVector.fromByte(OpCode.BrIf.toByte) ++ _)
        case BrTable(lbls, lbl) =>
          brtable
            .encode(lbls ~ lbl)
            .map(BitVector.fromByte(OpCode.BrTable.toByte) ++ _)
        case Return =>
          Attempt.successful(BitVector.fromByte(OpCode.Return.toByte))
        case Call(idx) =>
          varuint32.encode(idx).map(BitVector.fromByte(OpCode.Call.toByte) ++ _)
        case CallIndirect(idx) =>
          callindirect
            .encode(idx)
            .map(BitVector.fromByte(OpCode.CallIndirect.toByte) ++ _)
        case Drop => Attempt.successful(BitVector.fromByte(OpCode.Drop.toByte))
        case Select =>
          Attempt.successful(BitVector.fromByte(OpCode.Select.toByte))
        case LocalGet(idx) =>
          varuint32.encode(idx).map(BitVector.fromByte(inst.opcode.toByte) ++ _)
        case LocalSet(idx) =>
          varuint32.encode(idx).map(BitVector.fromByte(inst.opcode.toByte) ++ _)
        case LocalTee(idx) =>
          varuint32.encode(idx).map(BitVector.fromByte(inst.opcode.toByte) ++ _)
        case GlobalGet(idx) =>
          varuint32.encode(idx).map(BitVector.fromByte(inst.opcode.toByte) ++ _)
        case GlobalSet(idx) =>
          varuint32.encode(idx).map(BitVector.fromByte(inst.opcode.toByte) ++ _)
        case MemoryInst(align, offset) =>
          memarg
            .encode(align -> offset)
            .map(BitVector.fromByte(inst.opcode.toByte) ++ _)
        case MemorySize =>
          constant(hex"00")
            .encode(())
            .map(_ => BitVector.fromByte(OpCode.MemorySize.toByte))
        case MemoryGrow =>
          constant(hex"00")
            .encode(())
            .map(_ => BitVector.fromByte(OpCode.MemoryGrow.toByte))
        case i32.Const(v) =>
          varint32
            .encode(v)
            .map(BitVector.fromByte(OpCode.I32Const.toByte) ++ _)
        case i64.Const(v) =>
          varint64
            .encode(v)
            .map(BitVector.fromByte(OpCode.I64Const.toByte) ++ _)
        case f32.Const(v) =>
          floatL.encode(v).map(BitVector.fromByte(OpCode.F32Const.toByte) ++ _)
        case f64.Const(v) =>
          doubleL.encode(v).map(BitVector.fromByte(OpCode.F64Const.toByte) ++ _)
        case i32.Eqz =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Eqz.toByte))
        case i32.Eq =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Eq.toByte))
        case i32.Ne =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Ne.toByte))
        case i32.LtS =>
          Attempt.successful(BitVector.fromByte(OpCode.I32LtS.toByte))
        case i32.LtU =>
          Attempt.successful(BitVector.fromByte(OpCode.I32LtU.toByte))
        case i32.GtS =>
          Attempt.successful(BitVector.fromByte(OpCode.I32GtS.toByte))
        case i32.GtU =>
          Attempt.successful(BitVector.fromByte(OpCode.I32GtU.toByte))
        case i32.LeS =>
          Attempt.successful(BitVector.fromByte(OpCode.I32LeS.toByte))
        case i32.LeU =>
          Attempt.successful(BitVector.fromByte(OpCode.I32LeU.toByte))
        case i32.GeS =>
          Attempt.successful(BitVector.fromByte(OpCode.I32GeS.toByte))
        case i32.GeU =>
          Attempt.successful(BitVector.fromByte(OpCode.I32GeU.toByte))
        case i64.Eqz =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Eqz.toByte))
        case i64.Eq =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Eq.toByte))
        case i64.Ne =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Ne.toByte))
        case i64.LtS =>
          Attempt.successful(BitVector.fromByte(OpCode.I64LtS.toByte))
        case i64.LtU =>
          Attempt.successful(BitVector.fromByte(OpCode.I64LtU.toByte))
        case i64.GtS =>
          Attempt.successful(BitVector.fromByte(OpCode.I64GtS.toByte))
        case i64.GtU =>
          Attempt.successful(BitVector.fromByte(OpCode.I64GtU.toByte))
        case i64.LeS =>
          Attempt.successful(BitVector.fromByte(OpCode.I64LeS.toByte))
        case i64.LeU =>
          Attempt.successful(BitVector.fromByte(OpCode.I64LeU.toByte))
        case i64.GeS =>
          Attempt.successful(BitVector.fromByte(OpCode.I64GeS.toByte))
        case i64.GeU =>
          Attempt.successful(BitVector.fromByte(OpCode.I64GeU.toByte))
        case f32.Eq =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Eq.toByte))
        case f32.Ne =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Ne.toByte))
        case f32.Lt =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Lt.toByte))
        case f32.Gt =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Gt.toByte))
        case f32.Le =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Le.toByte))
        case f32.Ge =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Ge.toByte))
        case f64.Eq =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Eq.toByte))
        case f64.Ne =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Ne.toByte))
        case f64.Lt =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Lt.toByte))
        case f64.Gt =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Gt.toByte))
        case f64.Le =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Le.toByte))
        case f64.Ge =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Ge.toByte))
        case i32.Clz =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Clz.toByte))
        case i32.Ctz =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Ctz.toByte))
        case i32.Popcnt =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Popcnt.toByte))
        case i32.Add =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Add.toByte))
        case i32.Sub =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Sub.toByte))
        case i32.Mul =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Mul.toByte))
        case i32.DivS =>
          Attempt.successful(BitVector.fromByte(OpCode.I32DivS.toByte))
        case i32.DivU =>
          Attempt.successful(BitVector.fromByte(OpCode.I32DivU.toByte))
        case i32.RemS =>
          Attempt.successful(BitVector.fromByte(OpCode.I32RemS.toByte))
        case i32.RemU =>
          Attempt.successful(BitVector.fromByte(OpCode.I32RemU.toByte))
        case i32.And =>
          Attempt.successful(BitVector.fromByte(OpCode.I32And.toByte))
        case i32.Or =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Or.toByte))
        case i32.Xor =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Xor.toByte))
        case i32.Shl =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Shl.toByte))
        case i32.ShrS =>
          Attempt.successful(BitVector.fromByte(OpCode.I32ShrS.toByte))
        case i32.ShrU =>
          Attempt.successful(BitVector.fromByte(OpCode.I32ShrU.toByte))
        case i32.Rotl =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Rotl.toByte))
        case i32.Rotr =>
          Attempt.successful(BitVector.fromByte(OpCode.I32Rotr.toByte))
        case i64.Clz =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Clz.toByte))
        case i64.Ctz =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Ctz.toByte))
        case i64.Popcnt =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Popcnt.toByte))
        case i64.Add =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Add.toByte))
        case i64.Sub =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Sub.toByte))
        case i64.Mul =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Mul.toByte))
        case i64.DivS =>
          Attempt.successful(BitVector.fromByte(OpCode.I64DivS.toByte))
        case i64.DivU =>
          Attempt.successful(BitVector.fromByte(OpCode.I64DivU.toByte))
        case i64.RemS =>
          Attempt.successful(BitVector.fromByte(OpCode.I64RemS.toByte))
        case i64.RemU =>
          Attempt.successful(BitVector.fromByte(OpCode.I64RemU.toByte))
        case i64.And =>
          Attempt.successful(BitVector.fromByte(OpCode.I64And.toByte))
        case i64.Or =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Or.toByte))
        case i64.Xor =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Xor.toByte))
        case i64.Shl =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Shl.toByte))
        case i64.ShrS =>
          Attempt.successful(BitVector.fromByte(OpCode.I64ShrS.toByte))
        case i64.ShrU =>
          Attempt.successful(BitVector.fromByte(OpCode.I64ShrU.toByte))
        case i64.Rotl =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Rotl.toByte))
        case i64.Rotr =>
          Attempt.successful(BitVector.fromByte(OpCode.I64Rotr.toByte))
        case f32.Abs =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Abs.toByte))
        case f32.Neg =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Neg.toByte))
        case f32.Ceil =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Ceil.toByte))
        case f32.Floor =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Floor.toByte))
        case f32.Trunc =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Trunc.toByte))
        case f32.Nearest =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Nearest.toByte))
        case f32.Sqrt =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Sqrt.toByte))
        case f32.Add =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Add.toByte))
        case f32.Sub =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Sub.toByte))
        case f32.Mul =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Mul.toByte))
        case f32.Div =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Div.toByte))
        case f32.Min =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Min.toByte))
        case f32.Max =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Max.toByte))
        case f32.Copysign =>
          Attempt.successful(BitVector.fromByte(OpCode.F32Copysign.toByte))
        case f64.Abs =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Abs.toByte))
        case f64.Neg =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Neg.toByte))
        case f64.Ceil =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Ceil.toByte))
        case f64.Floor =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Floor.toByte))
        case f64.Trunc =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Trunc.toByte))
        case f64.Nearest =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Nearest.toByte))
        case f64.Sqrt =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Sqrt.toByte))
        case f64.Add =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Add.toByte))
        case f64.Sub =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Sub.toByte))
        case f64.Mul =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Mul.toByte))
        case f64.Div =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Div.toByte))
        case f64.Min =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Min.toByte))
        case f64.Max =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Max.toByte))
        case f64.Copysign =>
          Attempt.successful(BitVector.fromByte(OpCode.F64Copysign.toByte))
        case i32.WrapI64 =>
          Attempt.successful(BitVector.fromByte(OpCode.I32WrapI64.toByte))
        case i32.TruncSF32 =>
          Attempt.successful(BitVector.fromByte(OpCode.I32TruncSF32.toByte))
        case i32.TruncUF32 =>
          Attempt.successful(BitVector.fromByte(OpCode.I32TruncUF32.toByte))
        case i32.TruncSF64 =>
          Attempt.successful(BitVector.fromByte(OpCode.I32TruncSF64.toByte))
        case i32.TruncUF64 =>
          Attempt.successful(BitVector.fromByte(OpCode.I32TruncUF64.toByte))
        case i64.ExtendSI32 =>
          Attempt.successful(BitVector.fromByte(OpCode.I64ExtendSI32.toByte))
        case i64.ExtendUI32 =>
          Attempt.successful(BitVector.fromByte(OpCode.I64ExtendUI32.toByte))
        case i64.TruncSF32 =>
          Attempt.successful(BitVector.fromByte(OpCode.I64TruncSF32.toByte))
        case i64.TruncUF32 =>
          Attempt.successful(BitVector.fromByte(OpCode.I64TruncUF32.toByte))
        case i64.TruncSF64 =>
          Attempt.successful(BitVector.fromByte(OpCode.I64TruncSF64.toByte))
        case i64.TruncUF64 =>
          Attempt.successful(BitVector.fromByte(OpCode.I64TruncUF64.toByte))
        case f32.ConvertSI32 =>
          Attempt.successful(BitVector.fromByte(OpCode.F32ConvertSI32.toByte))
        case f32.ConvertUI32 =>
          Attempt.successful(BitVector.fromByte(OpCode.F32ConvertUI32.toByte))
        case f32.ConvertSI64 =>
          Attempt.successful(BitVector.fromByte(OpCode.F32ConvertSI64.toByte))
        case f32.ConvertUI64 =>
          Attempt.successful(BitVector.fromByte(OpCode.F32ConvertUI64.toByte))
        case f32.DemoteF64 =>
          Attempt.successful(BitVector.fromByte(OpCode.F32DemoteF64.toByte))
        case f64.ConvertSI32 =>
          Attempt.successful(BitVector.fromByte(OpCode.F64ConvertSI32.toByte))
        case f64.ConvertUI32 =>
          Attempt.successful(BitVector.fromByte(OpCode.F64ConvertUI32.toByte))
        case f64.ConvertSI64 =>
          Attempt.successful(BitVector.fromByte(OpCode.F64ConvertSI64.toByte))
        case f64.ConvertUI64 =>
          Attempt.successful(BitVector.fromByte(OpCode.F64ConvertUI64.toByte))
        case f64.PromoteF32 =>
          Attempt.successful(BitVector.fromByte(OpCode.F64PromoteF32.toByte))
        case i32.ReinterpretF32 =>
          Attempt.successful(BitVector.fromByte(OpCode.I32ReinterpretF32.toByte))
        case i64.ReinterpretF64 =>
          Attempt.successful(BitVector.fromByte(OpCode.I64ReinterpretF64.toByte))
        case f32.ReinterpretI32 =>
          Attempt.successful(BitVector.fromByte(OpCode.F32ReinterpretI32.toByte))
        case f64.ReinterpretI64 =>
          Attempt.successful(BitVector.fromByte(OpCode.F64ReinterpretI64.toByte))
        case _ => throw new RuntimeException(s"This is a bug: $inst")
      }

    def sizeBound: SizeBound =
      SizeBound.atLeast(8L)

  }

  private val instructions: Codec[Vector[Inst]] =
    vectorLookahead(lookahead(opcode.map(_ => ()).decodeOnly), instruction)

  val expr: Codec[Expr] =
    instructions <~ end

}
