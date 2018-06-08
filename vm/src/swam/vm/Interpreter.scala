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
package vm

import store._
import runtime._

import java.lang.{
  Integer => JInt,
  Long => JLong,
  Float => JFloat,
  Double => JDouble
}

import scala.annotation.{
  tailrec,
  switch
}

import java.nio.ByteBuffer

/** An interpreter is spwan each time the execution of a method is required. */
class Interpreter(private var frame: Frame, private val store: Store) {

  def interpret(): Result =
    run()

  @tailrec
  private def run(): Result = {
    ((frame.readByte() & 0xff): @switch) match {
      // === constants ===
      case OpCode.I32Const =>
        frame.stack.pushInt(frame.readInt())
        run()
      case OpCode.I64Const =>
        frame.stack.pushLong(frame.readLong())
        run()
      case OpCode.F32Const =>
        frame.stack.pushFloat(frame.readFloat())
        run()
      case OpCode.F64Const =>
        frame.stack.pushDouble(frame.readDouble())
        run()
      // === unary operators ===
      case OpCode.I32Clz =>
        frame.stack.pushInt(JInt.numberOfLeadingZeros(frame.stack.popInt()))
        run()
      case OpCode.I32Ctz =>
        frame.stack.pushInt(JInt.numberOfTrailingZeros(frame.stack.popInt()))
        run()
      case OpCode.I32Popcnt =>
        frame.stack.pushInt(JInt.bitCount(frame.stack.popInt()))
        run()
      case OpCode.I64Clz =>
        frame.stack.pushLong(JLong.numberOfLeadingZeros(frame.stack.popInt()))
        run()
      case OpCode.I64Ctz =>
        frame.stack.pushLong(JLong.numberOfTrailingZeros(frame.stack.popInt()))
        run()
      case OpCode.I64Popcnt =>
        frame.stack.pushLong(JLong.bitCount(frame.stack.popInt()))
        run()
      case OpCode.F32Abs =>
        frame.stack.pushFloat(StrictMath.abs(frame.stack.popFloat()))
        run()
      case OpCode.F32Neg =>
        frame.stack.pushFloat(-frame.stack.popFloat())
        run()
      case OpCode.F32Sqrt =>
        frame.stack.pushFloat(StrictMath.sqrt(frame.stack.popFloat()).toFloat)
        run()
      case OpCode.F32Ceil =>
        frame.stack.pushFloat(frame.stack.popFloat().ceil)
        run()
      case OpCode.F32Floor =>
        frame.stack.pushFloat(frame.stack.popFloat().floor)
        run()
      case OpCode.F32Trunc =>
        val f = frame.stack.popFloat()
        frame.stack.pushFloat(truncate(f))
        run()
      case OpCode.F32Nearest =>
        frame.stack.pushFloat(frame.stack.popFloat().round.toFloat)
        run()
      case OpCode.F64Abs =>
        frame.stack.pushDouble(StrictMath.abs(frame.stack.popDouble()))
        run()
      case OpCode.F64Neg =>
        frame.stack.pushDouble(-frame.stack.popDouble())
        run()
      case OpCode.F64Sqrt =>
        frame.stack.pushDouble(StrictMath.sqrt(frame.stack.popDouble()))
        run()
      case OpCode.F64Ceil =>
        frame.stack.pushDouble(frame.stack.popDouble().ceil)
        run()
      case OpCode.F64Floor =>
        frame.stack.pushDouble(frame.stack.popDouble().floor)
        run()
      case OpCode.F64Trunc =>
        val f = frame.stack.popDouble()
        frame.stack.pushDouble(truncate(f))
        run()
      case OpCode.F64Nearest =>
        frame.stack.pushDouble(StrictMath.rint(frame.stack.popDouble()))
        run()
      // === binary operators ===
      case OpCode.I32Add =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 + i2)
        run()
      case OpCode.I32Sub =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 - i2)
        run()
      case OpCode.I32Mul =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 * i2)
        run()
      case OpCode.I32DivU =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        if (i1 == 0) {
          Trap
        } else {
          frame.stack.pushInt(JInt.divideUnsigned(i1, i2))
          run()
        }
      case OpCode.I32DivS =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        if (i1 == 0) {
          Trap
        } else {
          val res = i1 / i2
          if (i1 >= 0 && i2 > 0 && res < 0) {
            Trap
          } else {
            frame.stack.pushInt(i1 / i2)
            run()
          }
        }
      case OpCode.I32RemU =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        if (i1 == 0) {
          Trap
        } else {
          frame.stack.pushInt(JInt.remainderUnsigned(i1, i2))
          run()
        }
      case OpCode.I32RemS =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        if (i1 == 0) {
          Trap
        } else {
          frame.stack.pushInt(i1 % i2)
          run()
        }
      case OpCode.I32And =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 & i2)
        run()
      case OpCode.I32Or =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 | i2)
        run()
      case OpCode.I32Xor =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 ^ i2)
        run()
      case OpCode.I32Shl =>
        val i2 = frame.stack.popInt() % 32
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 << i2)
        run()
      case OpCode.I32ShrU =>
        val i2 = frame.stack.popInt() % 32
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 >>> i2)
        run()
      case OpCode.I32ShrS =>
        val i2 = frame.stack.popInt() % 32
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(i1 >> i2)
        run()
      case OpCode.I32Rotl =>
        val i2 = frame.stack.popInt() % 32
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(JInt.rotateLeft(i1, i2))
        run()
      case OpCode.I32Rotr =>
        val i2 = frame.stack.popInt() % 32
        val i1 = frame.stack.popInt()
        frame.stack.pushInt(JInt.rotateRight(i1, i2))
        run()
      case OpCode.I64Add =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 + i2)
        run()
      case OpCode.I64Sub =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 - i2)
        run()
      case OpCode.I64Mul =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 * i2)
        run()
      case OpCode.I64DivU =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        if (i1 == 0) {
          Trap
        } else {
          frame.stack.pushLong(JLong.divideUnsigned(i1, i2))
          run()
        }
      case OpCode.I64DivS =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        if (i1 == 0) {
          Trap
        } else {
          val res = i1 / i2
          if (i1 >= 0 && i2 > 0 && res < 0) {
            Trap
          } else {
            frame.stack.pushLong(i1 / i2)
            run()
          }
        }
      case OpCode.I64RemU =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        if (i1 == 0) {
          Trap
        } else {
          frame.stack.pushLong(JLong.remainderUnsigned(i1, i2))
          run()
        }
      case OpCode.I64RemS =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        if (i1 == 0) {
          Trap
        } else {
          frame.stack.pushLong(i1 % i2)
          run()
        }
      case OpCode.I64And =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 & i2)
        run()
      case OpCode.I64Or =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 | i2)
        run()
      case OpCode.I64Xor =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 ^ i2)
        run()
      case OpCode.I64Shl =>
        val i2 = frame.stack.popLong() % 64
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 << i2)
        run()
      case OpCode.I64ShrU =>
        val i2 = frame.stack.popLong() % 64
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 >>> i2)
        run()
      case OpCode.I64ShrS =>
        val i2 = frame.stack.popLong() % 64
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(i1 >> i2)
        run()
      case OpCode.I64Rotl =>
        val i2 = (frame.stack.popLong() % 64).toInt
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(JLong.rotateLeft(i1, i2))
        run()
      case OpCode.I64Rotr =>
        val i2 = (frame.stack.popLong() % 64).toInt
        val i1 = frame.stack.popLong()
        frame.stack.pushLong(JLong.rotateRight(i1, i2))
        run()
      case OpCode.F32Add =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushFloat(f1 + f2)
        run()
      case OpCode.F32Sub =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushFloat(f1 - f2)
        run()
      case OpCode.F32Mul =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushFloat(f1 * f2)
        run()
      case OpCode.F32Div =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushFloat(f1 / f2)
        run()
      case OpCode.F32Min =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushFloat(StrictMath.min(f1, f2))
        run()
      case OpCode.F32Max =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushFloat(StrictMath.max(f1, f2))
        run()
      case OpCode.F32Copysign =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushFloat(StrictMath.copySign(f1, f2))
        run()
      case OpCode.F64Add =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushDouble(f1 + f2)
        run()
      case OpCode.F64Sub =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushDouble(f1 - f2)
        run()
      case OpCode.F64Mul =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushDouble(f1 * f2)
        run()
      case OpCode.F64Div =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushDouble(f1 / f2)
        run()
      case OpCode.F64Min =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushDouble(StrictMath.min(f1, f2))
        run()
      case OpCode.F64Max =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushDouble(StrictMath.max(f1, f2))
        run()
      case OpCode.F64Copysign =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushDouble(StrictMath.copySign(f1, f2))
        run()
      // === test operators ===
      case OpCode.I32Eqz =>
        val i = frame.stack.popInt()
        frame.stack.pushBool(i == 0)
        run()
      case OpCode.I64Eqz =>
        val i = frame.stack.popLong()
        frame.stack.pushBool(i == 0)
        run()
      // === relation operators ===
      case OpCode.I32Eq =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(i1 == i2)
        run()
      case OpCode.I32Ne =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(i1 != i2)
        run()
      case OpCode.I32LtU =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(JInt.compareUnsigned(i1, i2) < 0)
        run()
      case OpCode.I32LtS =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(i1 < i2)
        run()
      case OpCode.I32GtU =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(JInt.compareUnsigned(i1, i2) > 0)
        run()
      case OpCode.I32GtS =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(i1 > i2)
        run()
      case OpCode.I32LeU =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(JInt.compareUnsigned(i1, i2) <= 0)
        run()
      case OpCode.I32LeS =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(i1 >= i2)
        run()
      case OpCode.I32GeU =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(JInt.compareUnsigned(i1, i2) >= 0)
        run()
      case OpCode.I32GeS =>
        val i2 = frame.stack.popInt()
        val i1 = frame.stack.popInt()
        frame.stack.pushBool(i1 >= i2)
        run()
      case OpCode.I64Eq =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(i1 == i2)
        run()
      case OpCode.I64Ne =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(i1 != i2)
        run()
      case OpCode.I64LtU =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(JLong.compareUnsigned(i1, i2) < 0)
        run()
      case OpCode.I64LtS =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(i1 < i2)
        run()
      case OpCode.I64GtU =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(JLong.compareUnsigned(i1, i2) > 0)
        run()
      case OpCode.I64GtS =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(i1 > i2)
        run()
      case OpCode.I64LeU =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(JLong.compareUnsigned(i1, i2) <= 0)
        run()
      case OpCode.I64LeS =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(i1 >= i2)
        run()
      case OpCode.I64GeU =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(JLong.compareUnsigned(i1, i2) >= 0)
        run()
      case OpCode.I64GeS =>
        val i2 = frame.stack.popLong()
        val i1 = frame.stack.popLong()
        frame.stack.pushBool(i1 >= i2)
        run()
      case OpCode.F32Eq =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushBool(f1 == f2)
        run()
      case OpCode.F32Ne =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushBool(f1 != f2)
        run()
      case OpCode.F32Lt =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushBool(f1 < f2)
        run()
      case OpCode.F32Gt =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushBool(f1 > f2)
        run()
      case OpCode.F32Le =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushBool(f1 <= f2)
        run()
      case OpCode.F32Ge =>
        val f2 = frame.stack.popFloat()
        val f1 = frame.stack.popFloat()
        frame.stack.pushBool(f1 >= f2)
        run()
      case OpCode.F64Eq =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushBool(f1 == f2)
        run()
      case OpCode.F64Ne =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushBool(f1 != f2)
        run()
      case OpCode.F64Lt =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushBool(f1 < f2)
        run()
      case OpCode.F64Gt =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushBool(f1 > f2)
        run()
      case OpCode.F64Le =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushBool(f1 <= f2)
        run()
      case OpCode.F64Ge =>
        val f2 = frame.stack.popDouble()
        val f1 = frame.stack.popDouble()
        frame.stack.pushBool(f1 >= f2)
        run()
      // === conversion operators ===
      case OpCode.I32WrapI64 =>
        val l = frame.stack.popLong()
        frame.stack.pushInt((l % (1l << 32)).toInt)
        run()
      case OpCode.I64ExtendUI32 =>
        val i = frame.stack.popInt()
        frame.stack.pushLong(JInt.toUnsignedLong(i))
        run()
      case OpCode.I64ExtendSI32 =>
        val i = frame.stack.popInt()
        frame.stack.pushLong(i.toLong)
        run()
      case OpCode.I32TruncUF32 =>
        val f = frame.stack.popFloat()
        if (JFloat.isFinite(f)) {
          val t = truncate(f).toInt
          if (t >= 0) {
            frame.stack.pushInt(t)
            run()
          } else {
            Trap
          }
        } else {
          Trap
        }
      case OpCode.I32TruncSF32 =>
        val f = frame.stack.popFloat()
        val t = truncate(f)
        if (t.isValidInt) {
          frame.stack.pushInt(t.toInt)
          run()
        } else {
          Trap
        }
      case OpCode.I32TruncUF64 =>
        val f = frame.stack.popDouble()
        if (JDouble.isFinite(f)) {
          val t = truncate(f).toLong
          if (t >= 0 && t < (1l << 32)) {
            frame.stack.pushInt(t.toInt)
            run()
          } else {
            Trap
          }
        } else {
          Trap
        }
      case OpCode.I32TruncSF64 =>
        val f = frame.stack.popDouble()
        val t = truncate(f)
        if (t.isValidInt) {
          frame.stack.pushInt(t.toInt)
          run()
        } else {
          Trap
        }
      case OpCode.I64TruncUF32 =>
        val f = frame.stack.popFloat()
        if (JFloat.isFinite(f)) {
          val t = truncate(f).toLong
          if (t >= 0) {
            frame.stack.pushLong(t)
            run()
          } else {
            Trap
          }
        } else {
          Trap
        }
      case OpCode.I64TruncSF32 =>
        val f = frame.stack.popFloat()
        val t = truncate(f)
        if (t.isWhole) {
          frame.stack.pushLong(t.toLong)
          run()
        } else {
          Trap
        }
      case OpCode.I64TruncUF64 =>
        val f = frame.stack.popDouble()
        if (JDouble.isFinite(f)) {
          val t = truncate(f).toLong
          frame.stack.pushInt(t.toInt)
          run()
        } else {
          Trap
        }
      case OpCode.I64TruncSF64 =>
        val f = frame.stack.popDouble()
        val t = truncate(f)
        if (t.isWhole) {
          frame.stack.pushLong(t.toLong)
          run()
        } else {
          Trap
        }
      case OpCode.F32DemoteF64 =>
        val f = frame.stack.popDouble()
        frame.stack.pushFloat(f.toFloat)
        run()
      case OpCode.F64PromoteF32 =>
        val f = frame.stack.popFloat()
        frame.stack.pushDouble(f.toDouble)
        run()
      case OpCode.F32ConvertUI32 =>
        val i = frame.stack.popInt()
        frame.stack.pushFloat(JInt.toUnsignedLong(i).toFloat)
        run()
      case OpCode.F32ConvertSI32 =>
        val i = frame.stack.popInt()
        frame.stack.pushFloat(i.toFloat)
        run()
      case OpCode.F32ConvertUI64 =>
        val l = frame.stack.popLong()
        val f = BigDecimal(JLong.toUnsignedString(l)).floatValue
        frame.stack.pushFloat(f)
        run()
      case OpCode.F32ConvertSI64 =>
        val l = frame.stack.popLong()
        frame.stack.pushFloat(l.toFloat)
        run()
      case OpCode.F64ConvertUI32 =>
        val i = frame.stack.popInt()
        frame.stack.pushDouble(JInt.toUnsignedLong(i).toDouble)
        run()
      case OpCode.F64ConvertSI32 =>
        val i = frame.stack.popInt()
        frame.stack.pushDouble(i.toDouble)
        run()
      case OpCode.F64ConvertUI64 =>
        val l = frame.stack.popLong()
        val f = BigDecimal(JLong.toUnsignedString(l)).doubleValue
        frame.stack.pushDouble(f)
        run()
      case OpCode.F64ConvertSI64 =>
        val l = frame.stack.popLong()
        frame.stack.pushDouble(l.toDouble)
        run()
      case OpCode.I32ReinterpretF32 =>
        val f = frame.stack.popFloat()
        frame.stack.pushInt(JFloat.floatToRawIntBits(f))
        run()
      case OpCode.I64ReinterpretF64 =>
        val f = frame.stack.popDouble()
        frame.stack.pushLong(JDouble.doubleToRawLongBits(f))
        run()
      case OpCode.F32ReinterpretI32 =>
        val i = frame.stack.popInt()
        frame.stack.pushFloat(JFloat.intBitsToFloat(i))
        run()
      case OpCode.F64ReinterpretI64 =>
        val i = frame.stack.popLong()
        frame.stack.pushDouble(JDouble.longBitsToDouble(i))
        run()
      // === parameteric instructions ===
      case OpCode.Drop =>
        frame.stack.drop()
        run()
      case OpCode.Select =>
        val b = frame.stack.popBool()
        val v2 = frame.stack.popValue()
        val v1 = frame.stack.popValue()
        if (b)
          frame.stack.pushValue(v1)
        else
          frame.stack.pushValue(v2)
        run()
      case OpCode.GetLocal =>
        val idx = frame.readInt()
        frame.stack.pushValue(frame.locals(idx))
        run()
      case OpCode.SetLocal =>
        val idx = frame.readInt()
        val v = frame.stack.popValue()
        frame.locals(idx) = v
        run()
      case OpCode.TeeLocal =>
        val idx = frame.readInt()
        val v = frame.stack.popValue()
        frame.stack.pushValue(v)
        frame.stack.pushValue(v)
        frame.locals(idx) = v
        run()
      case OpCode.GetGlobal =>
        val idx = frame.readInt()
        frame.stack.pushValue(frame.instance.global(idx))
        run()
      case OpCode.SetGlobal =>
        val idx = frame.readInt()
        val v = frame.stack.popValue()
        frame.instance.global(idx) = v
        run()
      // === memory instructions ===
      case OpCode.I32Load =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c = mem.readInt(ea)
          frame.stack.pushInt(c)
          run()
        }
      case OpCode.I32Load8U =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c = mem.readByte(ea)
          frame.stack.pushInt(c & 0xff)
          run()
        }
      case OpCode.I32Load8S =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c = mem.readByte(ea)
          frame.stack.pushInt(c)
          run()
        }
      case OpCode.I32Load16U =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c = mem.readShort(ea)
          frame.stack.pushInt(c & 0xffff)
          run()
        }
      case OpCode.I32Load16S =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c = mem.readShort(ea)
          frame.stack.pushInt(c)
          run()
        }
      case OpCode.I64Load =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readLong(ea)
          frame.stack.pushLong(c)
          run()
        }
      case OpCode.I64Load8U =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readByte(ea)
          frame.stack.pushLong(c & 0xffl)
          run()
        }
      case OpCode.I64Load8S =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readByte(ea)
          frame.stack.pushLong(c)
          run()
        }
      case OpCode.I64Load16U =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readShort(ea)
          frame.stack.pushLong(c & 0xffffl)
          run()
        }
      case OpCode.I64Load16S =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readShort(ea)
          frame.stack.pushLong(c)
          run()
        }
      case OpCode.I64Load32U =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readInt(ea)
          frame.stack.pushLong(c & 0xffffffffl)
          run()
        }
      case OpCode.I64Load32S =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readInt(ea)
          frame.stack.pushLong(c)
          run()
        }
      case OpCode.F32Load =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c = mem.readFloat(ea)
          frame.stack.pushFloat(c)
          run()
        }
      case OpCode.F64Load =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c = mem.readDouble(ea)
          frame.stack.pushDouble(c)
          run()
        }
      case OpCode.I32Store =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popInt()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          mem.writeInt(ea, c)
          run()
        }
      case OpCode.I32Store8 =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popInt()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c1 = c % (1 << 8)
          mem.writeInt(ea, c1)
          run()
        }
      case OpCode.I32Store16 =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popInt()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          val c1 = c % (1 << 16)
          mem.writeInt(ea, c1)
          run()
        }
      case OpCode.I64Store =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popLong()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          mem.writeLong(ea, c)
          run()
        }
      case OpCode.I64Store8 =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popLong()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c1 = c % (1l << 8)
          mem.writeLong(ea, c1)
          run()
        }
      case OpCode.I64Store16 =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popLong()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c1 = c % (1l << 16)
          mem.writeLong(ea, c1)
          run()
        }
      case OpCode.I64Store32 =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popLong()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          val c1 = c % (1l << 32)
          mem.writeLong(ea, c1)
          run()
        }
      case OpCode.F32Store =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popFloat()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 4 > mem.size) {
          Trap
        } else {
          mem.writeFloat(ea, c)
          run()
        }
      case OpCode.F64Store =>
        val offset = frame.readInt()
        val mem = frame.instance.memory(0)
        val c = frame.stack.popDouble()
        val i = frame.stack.popInt()
        val ea = i + offset
        if (ea + 8 > mem.size) {
          Trap
        } else {
          mem.writeDouble(ea, c)
          run()
        }
      case OpCode.MemorySize =>
        val mem = frame.instance.memory(0)
        val sz = mem.size / pageSize
        frame.stack.pushInt(sz)
        run()
      case OpCode.MemoryGrow =>
        val mem = frame.instance.memory(0)
        val sz = mem.size / pageSize
        val n = frame.stack.popInt()
        if (mem.grow(n)) {
          frame.stack.pushInt(sz)
        } else {
          frame.stack.pushInt(-1)
        }
        run()
      // === control instructions ===
      case OpCode.Nop =>
        run()
      case OpCode.Unreachable =>
        Trap
      case OpCode.Block =>
        // next byte is the block return type
        val tpe = frame.readByte()
        // next int is the block size
        val blockSize = frame.readInt()
        val arity = tpe match {
          case 0x40 => 0
          case _    => 1
        }
        frame.stack.pushLabel(Label(arity, frame.pc + blockSize + 1 /* end */ ))
        // frame.pc is now on the first instruction of the block
        run()
      case OpCode.End =>
        // pop the m values on top of the stack
        val values = frame.stack.popValues()
        val lbl = frame.stack.popLabel()
        if (lbl.cont == -1) {
          // this is an implicit return at the end of a function
          // pop the frame to get the parent
          frame = frame.stack.popFrame()
        }
        // push values back
        frame.stack.pushValues(values)
        // frame.pc is now after the end of the block
        run()
      case OpCode.Loop =>
        // continuation will be the frame.pc of the loop opcode
        val cont = frame.pc - 1
        // skip the block return type
        frame.readByte()
        // push loop label
        frame.stack.pushLabel(Label(0, cont))
        // frame.pc is now on the first instruction of the loop
        run()
      case OpCode.If =>
        // next byte is the block return type
        val tpe = frame.readByte()
        // next int gives the size of the then branch in bytes
        val thenSize = frame.readInt()
        // next int gives the size of the else branch in bytes
        val elseSize = frame.readInt()
        val arity = tpe match {
          case 0x40 => 0
          case _    => 1
        }
        val c = frame.stack.popBool()
        val lbl = Label(arity, frame.pc + thenSize + 1 /* else */ + 4 /* else size */ + elseSize + 1 /* end */ )
        frame.stack.pushLabel(lbl)
        // frame.pc is now on the first instruction of the then branch
        if (!c) {
          // move frame.pc to the first instruction of the else branch
          frame.pc += thenSize + 1
        }
        run()
      case OpCode.Else =>
        // next int is the size of the else branch
        val elseSize = frame.readInt()
        // pop the m values on top of the stack
        val values = frame.stack.popValues()
        frame.stack.popLabel()
        // push values back
        frame.stack.pushValues(values)
        // put frame.pc after the end of the else block
        frame.pc += elseSize + 1 /* end */
        run()
      case OpCode.Br =>
        // next integer is the label
        val l = frame.readInt()
        // get the l-th label
        val lbl = frame.stack.getLabel(l)
        val arity = lbl.arity
        val cont = lbl.cont
        // pop the n values
        val values = frame.stack.popValues(arity)
        // pop all intermediate labels and values
        for (i <- 0 to l) {
          frame.stack.popValues()
          frame.stack.popLabel()
        }
        // push back return values
        frame.stack.pushValues(values)
        // jump to continuation
        frame.pc = cont
        run()
      case OpCode.BrIf =>
        // next integer is the label
        val l = frame.readInt()
        val c = frame.stack.popBool()
        if (c) {
          // get the l-th label
          val lbl = frame.stack.getLabel(l)
          val arity = lbl.arity
          val cont = lbl.cont
          // pop the n values
          val values = frame.stack.popValues(arity)
          // pop all intermediate labels and values
          for (i <- 0 to l) {
            frame.stack.popValues()
            frame.stack.popLabel()
          }
          // push back return values
          frame.stack.pushValues(values)
          // jump to continuation
          frame.pc = cont
        }
        run()
      case OpCode.BrTable =>
        // next int gives the number of labels to come
        val nl = frame.readInt()
        val lbls = for (_ <- 0 until nl) yield frame.readInt()
        // next comes the default label
        val ln = frame.readInt()
        // get the label index from stack
        val i = frame.stack.popInt()
        val l = if (i < lbls.size) lbls(i) else ln
        // get the l-th label
        val lbl = frame.stack.getLabel(l)
        val arity = lbl.arity
        val cont = lbl.cont
        // pop the n values
        val values = frame.stack.popValues(arity)
        // pop all intermediate labels and values
        for (i <- 0 to l) {
          frame.stack.popValues()
          frame.stack.popLabel()
        }
        // push back return values
        frame.stack.pushValues(values)
        // jump to continuation
        frame.pc = cont
        run()
      case OpCode.Return =>
        val values = frame.stack.popValues(frame.arity)
        // pop the frame to get the parent
        frame = frame.stack.popFrame()
        // push values back to the frame
        frame.stack.pushValues(values)
        // continue where we left the frame
        run()
      case OpCode.Call =>
        // next integer is the function index
        val fidx = frame.readInt()
        val f = frame.instance.function(fidx)
        invoke(f)
        run()
      case OpCode.CallIndirect =>
        // next integer is the typ index
        val tidx = frame.readInt()
        val tab = frame.instance.table(0)
        val expectedt = frame.instance.tpe(tidx)
        val i = frame.stack.popInt()
        if (i >= tab.size || tab(i) == NULL) {
          Trap
        } else {
          val f = store.function(tab(i))
          val actualt = f.tpe
          if (expectedt != actualt) {
            Trap
          } else {
            invoke(f)
            run()
          }
        }
      case opcode =>
        Trap
    }
  }

  private def invoke(f: FuncInstance): Unit = {
    val tpe = f.tpe
    val locals = Array.ofDim[Value](f.locals.size + tpe.params.size)
    val zlocals = f.locals.map(Value.zero(_))
    Array.copy(zlocals, 0, locals, tpe.params.size, zlocals.length)
    // pop the parameters from the stack
    val params = frame.stack.popValues(tpe.params.size).toArray
    Array.copy(params, 0, locals, 0, params.length)
    frame = frame.stack.pushFrame(tpe.t.size, f.code, locals)
    // push the implicit block label on the called frame
    frame.stack.pushLabel(Label(tpe.t.size, -1))
  }

}
