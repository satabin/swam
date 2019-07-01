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
package runtime
package internals
package interpreter
package low

import instance._

import java.lang.{Integer => JInt, Long => JLong, Float => JFloat, Double => JDouble}

import scala.annotation.switch

import java.nio.ByteBuffer

import cats._
import cats.implicits._

import scala.language.higherKinds
import scala.util.control.NonFatal

/** Interpreter of low-level assembly. */
private[runtime] class Interpreter[F[_]](engine: Engine[F])(implicit F: MonadError[F, Throwable])
    extends interpreter.Interpreter[F](engine) {

  private val conf = engine.conf

  def interpret(funcidx: Int, parameters: Vector[Long], instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level thread
    val thread = new ThreadFrame[F](conf.stack.low, instance)
    // push the parameters in the stack
    thread.pushValues(parameters)
    // invoke the function
    invoke(thread, instance.funcs(funcidx)) match {
      case Left(_)    => run(thread)
      case Right(res) => res
    }
  }

  def interpret(func: Function[F], parameters: Vector[Long], instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level thread
    val thread = new ThreadFrame[F](conf.stack.low, instance)
    // push the parameters in the stack
    thread.pushValues(parameters)
    // invoke the function
    invoke(thread, func) match {
      case Left(_)    => run(thread)
      case Right(res) => res
    }
  }

  def interpretInit(tpe: ValType, code: ByteBuffer, instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level thread
    val thread = new ThreadFrame[F](conf.stack.low, instance)
    // invoke the function
    invoke(thread, new FunctionInstance(FuncType(Vector(), Vector(tpe)), Vector(), code, instance)) match {
      case Left(_)    => run(thread)
      case Right(res) => res
    }
  }

  private def run(thread: ThreadFrame[F]): F[Option[Long]] = {
    def loop(): F[Option[Long]] = {
      val opcode = thread.readByte() & 0xff
      (opcode: @switch) match {
        // === constants ===
        case Asm.I32Const =>
          thread.pushInt(thread.readInt())
          loop()
        case Asm.I64Const =>
          thread.pushLong(thread.readLong())
          loop()
        case Asm.F32Const =>
          thread.pushFloat(thread.readFloat())
          loop()
        case Asm.F64Const =>
          thread.pushDouble(thread.readDouble())
          loop()
        // === unary operators ===
        case Asm.I32Clz =>
          thread.pushInt(JInt.numberOfLeadingZeros(thread.popInt()))
          loop()
        case Asm.I32Ctz =>
          thread.pushInt(JInt.numberOfTrailingZeros(thread.popInt()))
          loop()
        case Asm.I32Popcnt =>
          thread.pushInt(JInt.bitCount(thread.popInt()))
          loop()
        case Asm.I64Clz =>
          thread.pushLong(JLong.numberOfLeadingZeros(thread.popLong()))
          loop()
        case Asm.I64Ctz =>
          thread.pushLong(JLong.numberOfTrailingZeros(thread.popLong()))
          loop()
        case Asm.I64Popcnt =>
          thread.pushLong(JLong.bitCount(thread.popLong()))
          loop()
        case Asm.F32Abs =>
          thread.pushFloat(JFloat.intBitsToFloat(JFloat.floatToRawIntBits(thread.popFloat()) & 0x7fffffff))
          loop()
        case Asm.F32Neg =>
          thread.pushFloat(-thread.popFloat())
          loop()
        case Asm.F32Sqrt =>
          thread.pushFloat(StrictMath.sqrt(thread.popFloat()).toFloat)
          loop()
        case Asm.F32Ceil =>
          thread.pushFloat(thread.popFloat().ceil)
          loop()
        case Asm.F32Floor =>
          thread.pushFloat(thread.popFloat().floor)
          loop()
        case Asm.F32Trunc =>
          val f = thread.popFloat()
          thread.pushFloat(F32.trunc(f))
          loop()
        case Asm.F32Nearest =>
          val f = thread.popFloat()
          thread.pushFloat(F32.nearest(f))
          loop()
        case Asm.F64Abs =>
          thread.pushDouble(
            JDouble.longBitsToDouble(JDouble.doubleToRawLongBits(thread.popDouble()) & 0x7fffffffffffffffl))
          loop()
        case Asm.F64Neg =>
          thread.pushDouble(-thread.popDouble())
          loop()
        case Asm.F64Sqrt =>
          thread.pushDouble(StrictMath.sqrt(thread.popDouble()))
          loop()
        case Asm.F64Ceil =>
          thread.pushDouble(thread.popDouble().ceil)
          loop()
        case Asm.F64Floor =>
          thread.pushDouble(thread.popDouble().floor)
          loop()
        case Asm.F64Trunc =>
          val f = thread.popDouble()
          thread.pushDouble(F64.trunc(f))
          loop()
        case Asm.F64Nearest =>
          val d = thread.popDouble()
          thread.pushDouble(F64.nearest(d))
          loop()
        // === binary operators ===
        case Asm.I32Add =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushInt(i1 + i2)
          loop()
        case Asm.I32Sub =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushInt(i1 - i2)
          loop()
        case Asm.I32Mul =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushInt(i1 * i2)
          loop()
        case Asm.I32DivU =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else {
            thread.pushInt(JInt.divideUnsigned(i1, i2))
            loop()
          }
        case Asm.I32DivS =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else if (i1 == Int.MinValue && i2 == -1) {
            F.raiseError(new TrapException(thread, "integer overflow"))
          } else {
            val res = i1 / i2
            if (i1 >= 0 && i2 > 0 && res < 0) {
              F.raiseError(new TrapException(thread, "overflow"))
            } else {
              thread.pushInt(i1 / i2)
              loop()
            }
          }
        case Asm.I32RemU =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else {
            thread.pushInt(JInt.remainderUnsigned(i1, i2))
            loop()
          }
        case Asm.I32RemS =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else {
            thread.pushInt(i1 % i2)
            loop()
          }
        case Asm.I32And =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushInt(i1 & i2)
          loop()
        case Asm.I32Or =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushInt(i1 | i2)
          loop()
        case Asm.I32Xor =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushInt(i1 ^ i2)
          loop()
        case Asm.I32Shl =>
          val i2 = thread.popInt() % 32
          val i1 = thread.popInt()
          thread.pushInt(i1 << i2)
          loop()
        case Asm.I32ShrU =>
          val i2 = thread.popInt() % 32
          val i1 = thread.popInt()
          thread.pushInt(i1 >>> i2)
          loop()
        case Asm.I32ShrS =>
          val i2 = thread.popInt() % 32
          val i1 = thread.popInt()
          thread.pushInt(i1 >> i2)
          loop()
        case Asm.I32Rotl =>
          val i2 = thread.popInt() % 32
          val i1 = thread.popInt()
          thread.pushInt(JInt.rotateLeft(i1, i2))
          loop()
        case Asm.I32Rotr =>
          val i2 = thread.popInt() % 32
          val i1 = thread.popInt()
          thread.pushInt(JInt.rotateRight(i1, i2))
          loop()
        case Asm.I64Add =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushLong(i1 + i2)
          loop()
        case Asm.I64Sub =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushLong(i1 - i2)
          loop()
        case Asm.I64Mul =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushLong(i1 * i2)
          loop()
        case Asm.I64DivU =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else {
            thread.pushLong(JLong.divideUnsigned(i1, i2))
            loop()
          }
        case Asm.I64DivS =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else if (i1 == Long.MinValue && i2 == -1l) {
            F.raiseError(new TrapException(thread, "integer overflow"))
          } else {
            val res = i1 / i2
            if (i1 >= 0 && i2 > 0 && res < 0) {
              F.raiseError(new TrapException(thread, "overflow"))
            } else {
              thread.pushLong(i1 / i2)
              loop()
            }
          }
        case Asm.I64RemU =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else {
            thread.pushLong(JLong.remainderUnsigned(i1, i2))
            loop()
          }
        case Asm.I64RemS =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(thread, "integer divide by zero"))
          } else {
            thread.pushLong(i1 % i2)
            loop()
          }
        case Asm.I64And =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushLong(i1 & i2)
          loop()
        case Asm.I64Or =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushLong(i1 | i2)
          loop()
        case Asm.I64Xor =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushLong(i1 ^ i2)
          loop()
        case Asm.I64Shl =>
          val i2 = thread.popLong() % 64
          val i1 = thread.popLong()
          thread.pushLong(i1 << i2)
          loop()
        case Asm.I64ShrU =>
          val i2 = thread.popLong() % 64
          val i1 = thread.popLong()
          thread.pushLong(i1 >>> i2)
          loop()
        case Asm.I64ShrS =>
          val i2 = thread.popLong() % 64
          val i1 = thread.popLong()
          thread.pushLong(i1 >> i2)
          loop()
        case Asm.I64Rotl =>
          val i2 = (thread.popLong() % 64).toInt
          val i1 = thread.popLong()
          thread.pushLong(JLong.rotateLeft(i1, i2))
          loop()
        case Asm.I64Rotr =>
          val i2 = (thread.popLong() % 64).toInt
          val i1 = thread.popLong()
          thread.pushLong(JLong.rotateRight(i1, i2))
          loop()
        case Asm.F32Add =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushFloat(f1 + f2)
          loop()
        case Asm.F32Sub =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushFloat(f1 - f2)
          loop()
        case Asm.F32Mul =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushFloat(f1 * f2)
          loop()
        case Asm.F32Div =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushFloat(f1 / f2)
          loop()
        case Asm.F32Min =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushFloat(StrictMath.min(f1, f2))
          loop()
        case Asm.F32Max =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushFloat(StrictMath.max(f1, f2))
          loop()
        case Asm.F32Copysign =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushFloat(Math.copySign(f1, f2))
          loop()
        case Asm.F64Add =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushDouble(f1 + f2)
          loop()
        case Asm.F64Sub =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushDouble(f1 - f2)
          loop()
        case Asm.F64Mul =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushDouble(f1 * f2)
          loop()
        case Asm.F64Div =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushDouble(f1 / f2)
          loop()
        case Asm.F64Min =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushDouble(StrictMath.min(f1, f2))
          loop()
        case Asm.F64Max =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushDouble(StrictMath.max(f1, f2))
          loop()
        case Asm.F64Copysign =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushDouble(Math.copySign(f1, f2))
          loop()
        // === test operators ===
        case Asm.I32Eqz =>
          val i = thread.popInt()
          thread.pushBool(i == 0)
          loop()
        case Asm.I64Eqz =>
          val i = thread.popLong()
          thread.pushBool(i == 0)
          loop()
        // === relation operators ===
        case Asm.I32Eq =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(i1 == i2)
          loop()
        case Asm.I32Ne =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(i1 != i2)
          loop()
        case Asm.I32LtU =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(JInt.compareUnsigned(i1, i2) < 0)
          loop()
        case Asm.I32LtS =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(i1 < i2)
          loop()
        case Asm.I32GtU =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(JInt.compareUnsigned(i1, i2) > 0)
          loop()
        case Asm.I32GtS =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(i1 > i2)
          loop()
        case Asm.I32LeU =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(JInt.compareUnsigned(i1, i2) <= 0)
          loop()
        case Asm.I32LeS =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(i1 <= i2)
          loop()
        case Asm.I32GeU =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(JInt.compareUnsigned(i1, i2) >= 0)
          loop()
        case Asm.I32GeS =>
          val i2 = thread.popInt()
          val i1 = thread.popInt()
          thread.pushBool(i1 >= i2)
          loop()
        case Asm.I64Eq =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(i1 == i2)
          loop()
        case Asm.I64Ne =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(i1 != i2)
          loop()
        case Asm.I64LtU =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(JLong.compareUnsigned(i1, i2) < 0)
          loop()
        case Asm.I64LtS =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(i1 < i2)
          loop()
        case Asm.I64GtU =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(JLong.compareUnsigned(i1, i2) > 0)
          loop()
        case Asm.I64GtS =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(i1 > i2)
          loop()
        case Asm.I64LeU =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(JLong.compareUnsigned(i1, i2) <= 0)
          loop()
        case Asm.I64LeS =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(i1 <= i2)
          loop()
        case Asm.I64GeU =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(JLong.compareUnsigned(i1, i2) >= 0)
          loop()
        case Asm.I64GeS =>
          val i2 = thread.popLong()
          val i1 = thread.popLong()
          thread.pushBool(i1 >= i2)
          loop()
        case Asm.F32Eq =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushBool(f1 == f2)
          loop()
        case Asm.F32Ne =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushBool(f1 != f2)
          loop()
        case Asm.F32Lt =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushBool(f1 < f2)
          loop()
        case Asm.F32Gt =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushBool(f1 > f2)
          loop()
        case Asm.F32Le =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushBool(f1 <= f2)
          loop()
        case Asm.F32Ge =>
          val f2 = thread.popFloat()
          val f1 = thread.popFloat()
          thread.pushBool(f1 >= f2)
          loop()
        case Asm.F64Eq =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushBool(f1 == f2)
          loop()
        case Asm.F64Ne =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushBool(f1 != f2)
          loop()
        case Asm.F64Lt =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushBool(f1 < f2)
          loop()
        case Asm.F64Gt =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushBool(f1 > f2)
          loop()
        case Asm.F64Le =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushBool(f1 <= f2)
          loop()
        case Asm.F64Ge =>
          val f2 = thread.popDouble()
          val f1 = thread.popDouble()
          thread.pushBool(f1 >= f2)
          loop()
        // === conversion operators ===
        case Asm.I32WrapI64 =>
          val l = thread.popLong()
          thread.pushInt(I32.wrap(l))
          loop()
        case Asm.I64ExtendUI32 =>
          val i = thread.popInt()
          thread.pushLong(I64.extendUi32(i))
          loop()
        case Asm.I64ExtendSI32 =>
          val i = thread.popInt()
          thread.pushLong(I64.extendSi32(i))
          loop()
        case Asm.I32TruncUF32 =>
          val f = thread.popFloat()
          I32.truncUf32(f) match {
            case Right(i) =>
              thread.pushInt(i)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.I32TruncSF32 =>
          val f = thread.popFloat()
          I32.truncSf32(f) match {
            case Right(i) =>
              thread.pushInt(i)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.I32TruncUF64 =>
          val f = thread.popDouble()
          I32.truncUf64(f) match {
            case Right(i) =>
              thread.pushInt(i)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.I32TruncSF64 =>
          val f = thread.popDouble()
          I32.truncSf64(f) match {
            case Right(i) =>
              thread.pushInt(i)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.I64TruncUF32 =>
          val f = thread.popFloat()
          I64.truncUf32(f) match {
            case Right(l) =>
              thread.pushLong(l)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.I64TruncSF32 =>
          val f = thread.popFloat()
          I64.truncSf32(f) match {
            case Right(l) =>
              thread.pushLong(l)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.I64TruncUF64 =>
          val f = thread.popDouble()
          I64.truncUf64(f) match {
            case Right(l) =>
              thread.pushLong(l)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.I64TruncSF64 =>
          val f = thread.popDouble()
          I64.truncSf64(f) match {
            case Right(l) =>
              thread.pushLong(l)
              loop()
            case Left(msg) =>
              F.raiseError(new TrapException(thread, msg))
          }
        case Asm.F32DemoteF64 =>
          val f = thread.popDouble()
          thread.pushFloat(F32.demote(f))
          loop()
        case Asm.F64PromoteF32 =>
          val f = thread.popFloat()
          thread.pushDouble(F64.promote(f))
          loop()
        case Asm.F32ConvertUI32 =>
          val i = thread.popInt()
          thread.pushFloat(F32.convertUi32(i))
          loop()
        case Asm.F32ConvertSI32 =>
          val i = thread.popInt()
          thread.pushFloat(F32.convertSi32(i))
          loop()
        case Asm.F32ConvertUI64 =>
          val l = thread.popLong()
          thread.pushFloat(F32.convertUi64(l))
          loop()
        case Asm.F32ConvertSI64 =>
          val l = thread.popLong()
          thread.pushFloat(F32.convertSi64(l))
          loop()
        case Asm.F64ConvertUI32 =>
          val i = thread.popInt()
          thread.pushDouble(F64.convertUi32(i))
          loop()
        case Asm.F64ConvertSI32 =>
          val i = thread.popInt()
          thread.pushDouble(F64.convertSi32(i))
          loop()
        case Asm.F64ConvertUI64 =>
          val l = thread.popLong()
          thread.pushDouble(F64.convertUi64(l))
          loop()
        case Asm.F64ConvertSI64 =>
          val l = thread.popLong()
          thread.pushDouble(F64.convertSi64(l))
          loop()
        case Asm.I32ReinterpretF32 =>
          val f = thread.popFloat()
          thread.pushInt(I32.reinterpret(f))
          loop()
        case Asm.I64ReinterpretF64 =>
          val f = thread.popDouble()
          thread.pushLong(I64.reinterpret(f))
          loop()
        case Asm.F32ReinterpretI32 =>
          val i = thread.popInt()
          thread.pushFloat(F32.reinterpret(i))
          loop()
        case Asm.F64ReinterpretI64 =>
          val i = thread.popLong()
          thread.pushDouble(F64.reinterpret(i))
          loop()
        // === parameteric instructions ===
        case Asm.Drop =>
          val n = thread.readInt()
          thread.drop(n)
          loop()
        case Asm.Select =>
          val b = thread.popBool()
          val v2 = thread.popValue()
          val v1 = thread.popValue()
          if (b)
            thread.pushValue(v1)
          else
            thread.pushValue(v2)
          loop()
        case Asm.LocalGet =>
          val idx = thread.readInt()
          thread.pushValue(thread.local(idx))
          loop()
        case Asm.LocalSet =>
          val idx = thread.readInt()
          val v = thread.popValue()
          thread.setLocal(idx, v)
          loop()
        case Asm.LocalTee =>
          val idx = thread.readInt()
          val v = thread.peekValue()
          thread.setLocal(idx, v)
          loop()
        case Asm.GlobalGet =>
          val idx = thread.readInt()
          thread.global(idx) match {
            case i: GlobalInstance[F] =>
              thread.pushValue(i.rawget)
            case g =>
              thread.pushValue(Value.toRaw(g.get))
          }
          loop()
        case Asm.GlobalSet =>
          val idx = thread.readInt()
          val v = thread.popValue()
          thread.global(idx) match {
            case i: GlobalInstance[F] =>
              i.rawset(v)
              loop()
            case g =>
              g.set(Value.fromRaw(g.tpe.tpe, v))
              loop()
          }
        // === memory instructions ===
        case Asm.I32Load =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadInt(ea)
            thread.pushInt(c)
            loop()
          }
        case Asm.I32Load8U =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadByte(ea)
            thread.pushInt(c & 0xff)
            loop()
          }
        case Asm.I32Load8S =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadByte(ea)
            thread.pushInt(c)
            loop()
          }
        case Asm.I32Load16U =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadShort(ea)
            thread.pushInt(c & 0xffff)
            loop()
          }
        case Asm.I32Load16S =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadShort(ea)
            thread.pushInt(c)
            loop()
          }
        case Asm.I64Load =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadLong(ea)
            thread.pushLong(c)
            loop()
          }
        case Asm.I64Load8U =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadByte(ea)
            thread.pushLong(c & 0xffl)
            loop()
          }
        case Asm.I64Load8S =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadByte(ea)
            thread.pushLong(c)
            loop()
          }
        case Asm.I64Load16U =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadShort(ea)
            thread.pushLong(c & 0xffffl)
            loop()
          }
        case Asm.I64Load16S =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadShort(ea)
            thread.pushLong(c)
            loop()
          }
        case Asm.I64Load32U =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadInt(ea)
            thread.pushLong(c & 0xffffffffl)
            loop()
          }
        case Asm.I64Load32S =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadInt(ea)
            thread.pushLong(c)
            loop()
          }
        case Asm.F32Load =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadFloat(ea)
            thread.pushFloat(c)
            loop()
          }
        case Asm.F64Load =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c = mem.unsafeReadDouble(ea)
            thread.pushDouble(c)
            loop()
          }
        case Asm.I32Store =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popInt()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            mem.unsafeWriteInt(ea, c)
            loop()
          }
        case Asm.I32Store8 =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popInt()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c1 = (c % (1 << 8)).toByte
            mem.unsafeWriteByte(ea, c1)
            loop()
          }
        case Asm.I32Store16 =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popInt()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c1 = (c % (1 << 16)).toShort
            mem.unsafeWriteShort(ea, c1)
            loop()
          }
        case Asm.I64Store =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popLong()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            mem.unsafeWriteLong(ea, c)
            loop()
          }
        case Asm.I64Store8 =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popLong()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 8)).toByte
            mem.unsafeWriteByte(ea, c1)
            loop()
          }
        case Asm.I64Store16 =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popLong()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 16)).toShort
            mem.unsafeWriteShort(ea, c1)
            loop()
          }
        case Asm.I64Store32 =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popLong()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 32)).toInt
            mem.unsafeWriteInt(ea, c1)
            loop()
          }
        case Asm.F32Store =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popFloat()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            mem.unsafeWriteFloat(ea, c)
            loop()
          }
        case Asm.F64Store =>
          // ignore alignment for now
          thread.readInt()
          val offset = thread.readInt()
          val mem = thread.memory(0)
          val c = thread.popDouble()
          val i = thread.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new TrapException(thread, "out of bounds memory access"))
          } else {
            mem.unsafeWriteDouble(ea, c)
            loop()
          }
        case Asm.MemorySize =>
          val mem = thread.memory(0)
          val sz = mem.size / pageSize
          thread.pushInt(sz)
          loop()
        case Asm.MemoryGrow =>
          val mem = thread.memory(0)
          val sz = mem.size / pageSize
          val n = thread.popInt()
          if (mem.unsafeGrow(n))
            thread.pushInt(sz)
          else
            thread.pushInt(-1)
          loop()
        // === control instructions ===
        case Asm.Nop =>
          loop()
        case Asm.Unreachable =>
          F.raiseError(new TrapException(thread, "unreachable executed"))
        case Asm.Jump =>
          // next comes the jump address
          val addr = thread.readInt()
          thread.pc = addr
          loop()
        case Asm.JumpIf =>
          // read the condition from the stack
          val c = thread.popBool()
          if (c) {
            // only jump if condition is true
            // read the address
            val addr = thread.readInt()
            // and jump
            thread.pc = addr
          } else {
            // fix the pc and loop()
            thread.pc += 4
          }
          loop()
        case Asm.Br =>
          // next comes the label arity
          val arity = thread.readInt()
          // then the rest to drop
          val drop = thread.readInt()
          // and finally the jump address
          val addr = thread.readInt()
          br(thread, arity, drop, addr)
          loop()
        case Asm.BrIf =>
          val c = thread.popBool()
          if (c) {
            // only break if condition is true
            // next comes the label arity
            val arity = thread.readInt()
            // then the rest to drop
            val drop = thread.readInt()
            // and finally the jump address
            val addr = thread.readInt()
            br(thread, arity, drop, addr)
          } else {
            // otherwise increment program counter and loop()
            thread.pc += 12
          }
          loop()
        case Asm.BrTable =>
          // next int gives the number of labels to come
          val nl = thread.readInt()
          // get the label index from stack
          val i = thread.popInt()
          // fix the index to default to the default label
          val idx = if (i >= 0 && i < nl) i else nl
          // retrieve the correct label information at that index
          thread.pc += idx * 12
          val arity = thread.readInt()
          val drop = thread.readInt()
          val addr = thread.readInt()
          br(thread, arity, drop, addr)
          loop()
        case Asm.Return =>
          val values = thread.popValues(thread.arity)
          // pop the thread to get the parent
          thread.popFrame()
          if (thread.isToplevel) {
            // this is the top-level call, return, as we are done
            F.pure(values.headOption)
          } else {
            // push values back to the thread
            thread.pushValues(values)
            // loop() where we left the thread
            loop()
          }
        case Asm.Call =>
          // next integer is the function index
          val fidx = thread.readInt()
          val f = thread.func(fidx)
          invoke(thread, f) match {
            case Left(_)    => loop()
            case Right(res) => res
          }
        case Asm.CallIndirect =>
          // next integer is the typ index
          val tidx = thread.readInt()
          val tab = thread.table(0)
          val expectedt = thread.module.types(tidx)
          val i = thread.popInt()
          if (i < 0 || i >= tab.size) {
            F.raiseError(new TrapException(thread, "undefined element"))
          } else if (tab(i) == null) {
            F.raiseError(new TrapException(thread, s"uninitialized element $i"))
          } else {
            val f = tab(i)
            val actualt = f.tpe
            if (expectedt != actualt) {
              F.raiseError(new TrapException(thread, "indirect call type mismatch"))
            } else {
              invoke(thread, f) match {
                case Left(_)    => loop()
                case Right(res) => res
              }
            }
          }
        case opcode =>
          F.raiseError(new TrapException(thread, s"unknown opcode 0x${opcode.toHexString}"))
      }
    }

    try {
      loop()
    } catch {
      case e: ArrayIndexOutOfBoundsException => F.raiseError(new StackOverflowException(thread, e))
      case NonFatal(e)                       => F.raiseError(new TrapException(thread, "unexpected error during interpretation", e))
    }
  }

  private def br(thread: ThreadFrame[F], arity: Int, drop: Int, addr: Int): Unit = {
    val res = thread.popValues(arity)
    thread.drop(drop)
    thread.pushValues(res)
    thread.pc = addr
  }

  private val continue = Left(())

  private def invoke(thread: ThreadFrame[F], f: Function[F])(
      implicit F: MonadError[F, Throwable]): Either[Unit, F[Option[Long]]] =
    f match {
      case inst @ FunctionInstance(_, _, _, _) =>
        // parameters are on top of the stack
        thread.pushFrame(inst)
        continue
      case _ =>
        // pop the parameters from the stack
        val rawparams = thread.popValues(f.tpe.params.size)
        // convert parameters according to the type defined for function parameters
        val params = f.tpe.params.zip(rawparams).map {
          case (tpe, v) => Value.fromRaw(tpe, v)
        }
        // invoke the host function with the parameters
        Right(f.invoke(params.toVector, thread.memoryOpt(0)).flatMap { res =>
          if (thread.isToplevel) {
            F.pure(res.map(Value.toRaw(_)))
          } else {
            res.foreach(v => thread.pushValue(Value.toRaw(v)))
            run(thread)
          }
        })
    }

}
