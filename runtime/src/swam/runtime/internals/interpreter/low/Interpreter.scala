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

import scala.annotation.{tailrec, switch}

import java.nio.ByteBuffer

import cats._
import cats.implicits._

import scala.language.higherKinds

/** Interpreter of low-level assembly. */
private[runtime] class Interpreter[F[_]](engine: Engine[F])(implicit F: MonadError[F, Throwable]) extends interpreter.Interpreter[F](engine) {

  private val conf = engine.conf

  def interpret(funcidx: Int, parameters: Vector[Long], instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level frame
    val frame = Frame.makeToplevel[F](instance, conf)
    // push the parameters in the stack
    frame.stack.pushValues(parameters)
    // invoke the function
    invoke(frame, instance.funcs(funcidx)).flatMap {
      case Left(frame) => run(frame)
      case Right(res)  => F.pure(res)
    }
  }

  def interpret(func: Function[F], parameters: Vector[Long], instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level frame
    val frame = Frame.makeToplevel[F](instance, conf)
    // push the parameters in the stack
    frame.stack.pushValues(parameters)
    // invoke the function
    invoke(frame, func).flatMap {
      case Left(frame) => run(frame)
      case Right(res)  => F.pure(res)
    }
  }

  def interpretInit(tpe: ValType, code: ByteBuffer, instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level frame
    val frame = Frame.makeToplevel[F](instance, conf)
    // invoke the function
    invoke(frame, new FunctionInstance(FuncType(Vector(), Vector(tpe)), Vector(), code, instance)).flatMap {
      case Left(frame) => run(frame)
      case Right(res)  => F.pure(res)
    }
  }

  private def run(frame: Frame[F]): F[Option[Long]] =
    F.tailRecM(frame) { frame =>
      val opcode = frame.readByte() & 0xff
      (opcode: @switch) match {
        // === constants ===
        case Asm.I32Const =>
          frame.stack.pushInt(frame.readInt())
          F.pure(Left(frame))
        case Asm.I64Const =>
          frame.stack.pushLong(frame.readLong())
          F.pure(Left(frame))
        case Asm.F32Const =>
          frame.stack.pushFloat(frame.readFloat())
          F.pure(Left(frame))
        case Asm.F64Const =>
          frame.stack.pushDouble(frame.readDouble())
          F.pure(Left(frame))
        // === unary operators ===
        case Asm.I32Clz =>
          frame.stack.pushInt(JInt.numberOfLeadingZeros(frame.stack.popInt()))
          F.pure(Left(frame))
        case Asm.I32Ctz =>
          frame.stack.pushInt(JInt.numberOfTrailingZeros(frame.stack.popInt()))
          F.pure(Left(frame))
        case Asm.I32Popcnt =>
          frame.stack.pushInt(JInt.bitCount(frame.stack.popInt()))
          F.pure(Left(frame))
        case Asm.I64Clz =>
          frame.stack.pushLong(JLong.numberOfLeadingZeros(frame.stack.popLong()))
          F.pure(Left(frame))
        case Asm.I64Ctz =>
          frame.stack.pushLong(JLong.numberOfTrailingZeros(frame.stack.popLong()))
          F.pure(Left(frame))
        case Asm.I64Popcnt =>
          frame.stack.pushLong(JLong.bitCount(frame.stack.popLong()))
          F.pure(Left(frame))
        case Asm.F32Abs =>
          frame.stack.pushFloat(JFloat.intBitsToFloat(JFloat.floatToRawIntBits(frame.stack.popFloat()) & 0x7fffffff))
          F.pure(Left(frame))
        case Asm.F32Neg =>
          frame.stack.pushFloat(-frame.stack.popFloat())
          F.pure(Left(frame))
        case Asm.F32Sqrt =>
          frame.stack.pushFloat(StrictMath.sqrt(frame.stack.popFloat()).toFloat)
          F.pure(Left(frame))
        case Asm.F32Ceil =>
          frame.stack.pushFloat(frame.stack.popFloat().ceil)
          F.pure(Left(frame))
        case Asm.F32Floor =>
          frame.stack.pushFloat(frame.stack.popFloat().floor)
          F.pure(Left(frame))
        case Asm.F32Trunc =>
          val f = frame.stack.popFloat()
          frame.stack.pushFloat(F32.trunc(f))
          F.pure(Left(frame))
        case Asm.F32Nearest =>
          val f = frame.stack.popFloat()
          frame.stack.pushFloat(F32.nearest(f))
          F.pure(Left(frame))
        case Asm.F64Abs =>
          frame.stack.pushDouble(
            JDouble.longBitsToDouble(JDouble.doubleToRawLongBits(frame.stack.popDouble()) & 0x7fffffffffffffffl))
          F.pure(Left(frame))
        case Asm.F64Neg =>
          frame.stack.pushDouble(-frame.stack.popDouble())
          F.pure(Left(frame))
        case Asm.F64Sqrt =>
          frame.stack.pushDouble(StrictMath.sqrt(frame.stack.popDouble()))
          F.pure(Left(frame))
        case Asm.F64Ceil =>
          frame.stack.pushDouble(frame.stack.popDouble().ceil)
          F.pure(Left(frame))
        case Asm.F64Floor =>
          frame.stack.pushDouble(frame.stack.popDouble().floor)
          F.pure(Left(frame))
        case Asm.F64Trunc =>
          val f = frame.stack.popDouble()
          frame.stack.pushDouble(F64.trunc(f))
          F.pure(Left(frame))
        case Asm.F64Nearest =>
          val d = frame.stack.popDouble()
          frame.stack.pushDouble(F64.nearest(d))
          F.pure(Left(frame))
        // === binary operators ===
        case Asm.I32Add =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 + i2)
          F.pure(Left(frame))
        case Asm.I32Sub =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 - i2)
          F.pure(Left(frame))
        case Asm.I32Mul =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 * i2)
          F.pure(Left(frame))
        case Asm.I32DivU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushInt(JInt.divideUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case Asm.I32DivS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else if (i1 == Int.MinValue && i2 == -1) {
            F.raiseError(new TrapException(frame, "integer overflow"))
          } else {
            val res = i1 / i2
            if (i1 >= 0 && i2 > 0 && res < 0) {
              F.raiseError(new TrapException(frame, "overflow"))
            } else {
              frame.stack.pushInt(i1 / i2)
              F.pure(Left(frame))
            }
          }
        case Asm.I32RemU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushInt(JInt.remainderUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case Asm.I32RemS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushInt(i1 % i2)
            F.pure(Left(frame))
          }
        case Asm.I32And =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 & i2)
          F.pure(Left(frame))
        case Asm.I32Or =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 | i2)
          F.pure(Left(frame))
        case Asm.I32Xor =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 ^ i2)
          F.pure(Left(frame))
        case Asm.I32Shl =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 << i2)
          F.pure(Left(frame))
        case Asm.I32ShrU =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 >>> i2)
          F.pure(Left(frame))
        case Asm.I32ShrS =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 >> i2)
          F.pure(Left(frame))
        case Asm.I32Rotl =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(JInt.rotateLeft(i1, i2))
          F.pure(Left(frame))
        case Asm.I32Rotr =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(JInt.rotateRight(i1, i2))
          F.pure(Left(frame))
        case Asm.I64Add =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 + i2)
          F.pure(Left(frame))
        case Asm.I64Sub =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 - i2)
          F.pure(Left(frame))
        case Asm.I64Mul =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 * i2)
          F.pure(Left(frame))
        case Asm.I64DivU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushLong(JLong.divideUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case Asm.I64DivS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else if (i1 == Long.MinValue && i2 == -1l) {
            F.raiseError(new TrapException(frame, "integer overflow"))
          } else {
            val res = i1 / i2
            if (i1 >= 0 && i2 > 0 && res < 0) {
              F.raiseError(new TrapException(frame, "overflow"))
            } else {
              frame.stack.pushLong(i1 / i2)
              F.pure(Left(frame))
            }
          }
        case Asm.I64RemU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushLong(JLong.remainderUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case Asm.I64RemS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new TrapException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushLong(i1 % i2)
            F.pure(Left(frame))
          }
        case Asm.I64And =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 & i2)
          F.pure(Left(frame))
        case Asm.I64Or =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 | i2)
          F.pure(Left(frame))
        case Asm.I64Xor =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 ^ i2)
          F.pure(Left(frame))
        case Asm.I64Shl =>
          val i2 = frame.stack.popLong() % 64
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 << i2)
          F.pure(Left(frame))
        case Asm.I64ShrU =>
          val i2 = frame.stack.popLong() % 64
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 >>> i2)
          F.pure(Left(frame))
        case Asm.I64ShrS =>
          val i2 = frame.stack.popLong() % 64
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 >> i2)
          F.pure(Left(frame))
        case Asm.I64Rotl =>
          val i2 = (frame.stack.popLong() % 64).toInt
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(JLong.rotateLeft(i1, i2))
          F.pure(Left(frame))
        case Asm.I64Rotr =>
          val i2 = (frame.stack.popLong() % 64).toInt
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(JLong.rotateRight(i1, i2))
          F.pure(Left(frame))
        case Asm.F32Add =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 + f2)
          F.pure(Left(frame))
        case Asm.F32Sub =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 - f2)
          F.pure(Left(frame))
        case Asm.F32Mul =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 * f2)
          F.pure(Left(frame))
        case Asm.F32Div =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 / f2)
          F.pure(Left(frame))
        case Asm.F32Min =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(StrictMath.min(f1, f2))
          F.pure(Left(frame))
        case Asm.F32Max =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(StrictMath.max(f1, f2))
          F.pure(Left(frame))
        case Asm.F32Copysign =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(Math.copySign(f1, f2))
          F.pure(Left(frame))
        case Asm.F64Add =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 + f2)
          F.pure(Left(frame))
        case Asm.F64Sub =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 - f2)
          F.pure(Left(frame))
        case Asm.F64Mul =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 * f2)
          F.pure(Left(frame))
        case Asm.F64Div =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 / f2)
          F.pure(Left(frame))
        case Asm.F64Min =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(StrictMath.min(f1, f2))
          F.pure(Left(frame))
        case Asm.F64Max =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(StrictMath.max(f1, f2))
          F.pure(Left(frame))
        case Asm.F64Copysign =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(Math.copySign(f1, f2))
          F.pure(Left(frame))
        // === test operators ===
        case Asm.I32Eqz =>
          val i = frame.stack.popInt()
          frame.stack.pushBool(i == 0)
          F.pure(Left(frame))
        case Asm.I64Eqz =>
          val i = frame.stack.popLong()
          frame.stack.pushBool(i == 0)
          F.pure(Left(frame))
        // === relation operators ===
        case Asm.I32Eq =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 == i2)
          F.pure(Left(frame))
        case Asm.I32Ne =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 != i2)
          F.pure(Left(frame))
        case Asm.I32LtU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) < 0)
          F.pure(Left(frame))
        case Asm.I32LtS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 < i2)
          F.pure(Left(frame))
        case Asm.I32GtU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) > 0)
          F.pure(Left(frame))
        case Asm.I32GtS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 > i2)
          F.pure(Left(frame))
        case Asm.I32LeU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) <= 0)
          F.pure(Left(frame))
        case Asm.I32LeS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 <= i2)
          F.pure(Left(frame))
        case Asm.I32GeU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) >= 0)
          F.pure(Left(frame))
        case Asm.I32GeS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 >= i2)
          F.pure(Left(frame))
        case Asm.I64Eq =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 == i2)
          F.pure(Left(frame))
        case Asm.I64Ne =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 != i2)
          F.pure(Left(frame))
        case Asm.I64LtU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) < 0)
          F.pure(Left(frame))
        case Asm.I64LtS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 < i2)
          F.pure(Left(frame))
        case Asm.I64GtU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) > 0)
          F.pure(Left(frame))
        case Asm.I64GtS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 > i2)
          F.pure(Left(frame))
        case Asm.I64LeU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) <= 0)
          F.pure(Left(frame))
        case Asm.I64LeS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 <= i2)
          F.pure(Left(frame))
        case Asm.I64GeU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) >= 0)
          F.pure(Left(frame))
        case Asm.I64GeS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 >= i2)
          F.pure(Left(frame))
        case Asm.F32Eq =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 == f2)
          F.pure(Left(frame))
        case Asm.F32Ne =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 != f2)
          F.pure(Left(frame))
        case Asm.F32Lt =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 < f2)
          F.pure(Left(frame))
        case Asm.F32Gt =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 > f2)
          F.pure(Left(frame))
        case Asm.F32Le =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 <= f2)
          F.pure(Left(frame))
        case Asm.F32Ge =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 >= f2)
          F.pure(Left(frame))
        case Asm.F64Eq =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 == f2)
          F.pure(Left(frame))
        case Asm.F64Ne =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 != f2)
          F.pure(Left(frame))
        case Asm.F64Lt =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 < f2)
          F.pure(Left(frame))
        case Asm.F64Gt =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 > f2)
          F.pure(Left(frame))
        case Asm.F64Le =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 <= f2)
          F.pure(Left(frame))
        case Asm.F64Ge =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 >= f2)
          F.pure(Left(frame))
        // === conversion operators ===
        case Asm.I32WrapI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushInt(I32.wrap(l))
          F.pure(Left(frame))
        case Asm.I64ExtendUI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushLong(I64.extendUi32(i))
          F.pure(Left(frame))
        case Asm.I64ExtendSI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushLong(I64.extendSi32(i))
          F.pure(Left(frame))
        case Asm.I32TruncUF32 =>
          val f = frame.stack.popFloat()
          I32.truncUf32(f) match {
            case Right(i) =>
              frame.stack.pushInt(i)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.I32TruncSF32 =>
          val f = frame.stack.popFloat()
          I32.truncSf32(f) match {
            case Right(i) =>
              frame.stack.pushInt(i)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.I32TruncUF64 =>
          val f = frame.stack.popDouble()
          I32.truncUf64(f) match {
            case Right(i) =>
              frame.stack.pushInt(i)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.I32TruncSF64 =>
          val f = frame.stack.popDouble()
          I32.truncSf64(f) match {
            case Right(i) =>
              frame.stack.pushInt(i)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.I64TruncUF32 =>
          val f = frame.stack.popFloat()
          I64.truncUf32(f) match {
            case Right(l) =>
              frame.stack.pushLong(l)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.I64TruncSF32 =>
          val f = frame.stack.popFloat()
          I64.truncSf32(f) match {
            case Right(l) =>
              frame.stack.pushLong(l)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.I64TruncUF64 =>
          val f = frame.stack.popDouble()
          I64.truncUf64(f) match {
            case Right(l) =>
              frame.stack.pushLong(l)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.I64TruncSF64 =>
          val f = frame.stack.popDouble()
          I64.truncSf64(f) match {
            case Right(l) =>
              frame.stack.pushLong(l)
              F.pure(Left(frame))
            case Left(msg) =>
              F.raiseError(new TrapException(frame, msg))
          }
        case Asm.F32DemoteF64 =>
          val f = frame.stack.popDouble()
          frame.stack.pushFloat(F32.demote(f))
          F.pure(Left(frame))
        case Asm.F64PromoteF32 =>
          val f = frame.stack.popFloat()
          frame.stack.pushDouble(F64.promote(f))
          F.pure(Left(frame))
        case Asm.F32ConvertUI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushFloat(F32.convertUi32(i))
          F.pure(Left(frame))
        case Asm.F32ConvertSI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushFloat(F32.convertSi32(i))
          F.pure(Left(frame))
        case Asm.F32ConvertUI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushFloat(F32.convertUi64(l))
          F.pure(Left(frame))
        case Asm.F32ConvertSI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushFloat(F32.convertSi64(l))
          F.pure(Left(frame))
        case Asm.F64ConvertUI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushDouble(F64.convertUi32(i))
          F.pure(Left(frame))
        case Asm.F64ConvertSI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushDouble(F64.convertSi32(i))
          F.pure(Left(frame))
        case Asm.F64ConvertUI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushDouble(F64.convertUi64(l))
          F.pure(Left(frame))
        case Asm.F64ConvertSI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushDouble(F64.convertSi64(l))
          F.pure(Left(frame))
        case Asm.I32ReinterpretF32 =>
          val f = frame.stack.popFloat()
          frame.stack.pushInt(I32.reinterpret(f))
          F.pure(Left(frame))
        case Asm.I64ReinterpretF64 =>
          val f = frame.stack.popDouble()
          frame.stack.pushLong(I64.reinterpret(f))
          F.pure(Left(frame))
        case Asm.F32ReinterpretI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushFloat(F32.reinterpret(i))
          F.pure(Left(frame))
        case Asm.F64ReinterpretI64 =>
          val i = frame.stack.popLong()
          frame.stack.pushDouble(F64.reinterpret(i))
          F.pure(Left(frame))
        // === parameteric instructions ===
        case Asm.Drop =>
          val n = frame.readInt()
          frame.stack.drop(n)
          F.pure(Left(frame))
        case Asm.Select =>
          val b = frame.stack.popBool()
          val v2 = frame.stack.popValue()
          val v1 = frame.stack.popValue()
          if (b)
            frame.stack.pushValue(v1)
          else
            frame.stack.pushValue(v2)
          F.pure(Left(frame))
        case Asm.LocalGet =>
          val idx = frame.readInt()
          frame.stack.pushValue(frame.locals(idx))
          F.pure(Left(frame))
        case Asm.LocalSet =>
          val idx = frame.readInt()
          val v = frame.stack.popValue()
          frame.locals(idx) = v
          F.pure(Left(frame))
        case Asm.LocalTee =>
          val idx = frame.readInt()
          val v = frame.stack.peekValue()
          frame.locals(idx) = v
          F.pure(Left(frame))
        case Asm.GlobalGet =>
          val idx = frame.readInt()
          frame.instance.globals(idx) match {
            case i: GlobalInstance[F] =>
              frame.stack.pushValue(i.rawget)
            case g =>
              frame.stack.pushValue(Value.toRaw(g.get))
          }
          F.pure(Left(frame))
        case Asm.GlobalSet =>
          val idx = frame.readInt()
          val v = frame.stack.popValue()
          frame.instance.globals(idx) match {
            case i: GlobalInstance[F] =>
              i.rawset(v)
              F.pure(Left(frame))
            case g =>
              g.set(Value.fromRaw(g.tpe.tpe, v)) >> F.pure(Left(frame))
          }
        // === memory instructions ===
        case Asm.I32Load =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readInt(ea).map { c =>
              frame.stack.pushInt(c)
              Left(frame)
            }
        case Asm.I32Load8U =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readByte(ea).map { c =>
              frame.stack.pushInt(c & 0xff)
              Left(frame)
            }
        case Asm.I32Load8S =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readByte(ea).map { c =>
              frame.stack.pushInt(c)
              Left(frame)
            }
        case Asm.I32Load16U =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readShort(ea).map { c =>
              frame.stack.pushInt(c & 0xffff)
              Left(frame)
            }
        case Asm.I32Load16S =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readShort(ea).map { c =>
              frame.stack.pushInt(c)
              Left(frame)
            }
        case Asm.I64Load =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readLong(ea).map { c =>
              frame.stack.pushLong(c)
              Left(frame)
            }
        case Asm.I64Load8U =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readByte(ea).map { c =>
              frame.stack.pushLong(c & 0xffl)
              Left(frame)
            }
        case Asm.I64Load8S =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readByte(ea).map { c =>
              frame.stack.pushLong(c)
              Left(frame)
            }
        case Asm.I64Load16U =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readShort(ea).map { c =>
              frame.stack.pushLong(c & 0xffffl)
              Left(frame)
            }
        case Asm.I64Load16S =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readShort(ea).map { c =>
              frame.stack.pushLong(c)
              Left(frame)
            }
        case Asm.I64Load32U =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readInt(ea).map { c =>
              frame.stack.pushLong(c & 0xffffffffl)
              Left(frame)
            }
        case Asm.I64Load32S =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readInt(ea).map { c =>
              frame.stack.pushLong(c)
              Left(frame)
            }
        case Asm.F32Load =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readFloat(ea).map { c =>
              frame.stack.pushFloat(c)
              Left(frame)
            }
        case Asm.F64Load =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.readDouble(ea).map { c =>
              frame.stack.pushDouble(c)
              Left(frame)
            }
        case Asm.I32Store =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popInt()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size)
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          else
            mem.writeInt(ea, c).as(Left(frame))
        case Asm.I32Store8 =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popInt()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1 << 8)).toByte
            mem.writeByte(ea, c1).as(Left(frame))
          }
        case Asm.I32Store16 =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popInt()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1 << 16)).toShort
            mem.writeShort(ea, c1).as(Left(frame))
          }
        case Asm.I64Store =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            mem.writeLong(ea, c).as(Left(frame))
          }
        case Asm.I64Store8 =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 8)).toByte
            mem.writeByte(ea, c1).as(Left(frame))
          }
        case Asm.I64Store16 =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 16)).toShort
            mem.writeShort(ea, c1).as(Left(frame))
          }
        case Asm.I64Store32 =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 32)).toInt
            mem.writeInt(ea, c1).as(Left(frame))
          }
        case Asm.F32Store =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popFloat()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            mem.writeFloat(ea, c).as(Left(frame))
          }
        case Asm.F64Store =>
          val align = frame.readInt()
          val offset = frame.readInt()
          val mem = frame.instance.memories(0)
          val c = frame.stack.popDouble()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new TrapException(frame, "out of bounds memory access"))
          } else {
            mem.writeDouble(ea, c).as(Left(frame))
          }
        case Asm.MemorySize =>
          val mem = frame.instance.memories(0)
          val sz = mem.size / pageSize
          frame.stack.pushInt(sz)
          F.pure(Left(frame))
        case Asm.MemoryGrow =>
          val mem = frame.instance.memories(0)
          val sz = mem.size / pageSize
          val n = frame.stack.popInt()
          mem
            .grow(n)
            .map {
              case true  => frame.stack.pushInt(sz)
              case flase => frame.stack.pushInt(-1)
            }
            .as(Left(frame))
        // === control instructions ===
        case Asm.Nop =>
          F.pure(Left(frame))
        case Asm.Unreachable =>
          F.raiseError(new TrapException(frame, "unreachable executed"))
        case Asm.Jump =>
          // next comes the jump address
          val addr = frame.readInt()
          frame.pc = addr
          F.pure(Left(frame))
        case Asm.JumpIf =>
          // read the condition from the stack
          val c = frame.stack.popBool()
          if (c) {
            // only jump if condition is true
            // read the address
            val addr = frame.readInt()
            // and jump
            frame.pc = addr
          } else {
            // fix the pc and continue
            frame.pc += 4
          }
          F.pure(Left(frame))
        case Asm.Br =>
          // next comes the label arity
          val arity = frame.readInt()
          // then the rest to drop
          val drop = frame.readInt()
          // and finally the jump address
          val addr = frame.readInt()
          br(frame, arity, drop, addr)
          F.pure(Left(frame))
        case Asm.BrIf =>
          val c = frame.stack.popBool()
          if (c) {
            // only break if condition is true
            // next comes the label arity
            val arity = frame.readInt()
            // then the rest to drop
            val drop = frame.readInt()
            // and finally the jump address
            val addr = frame.readInt()
            br(frame, arity, drop, addr)
          } else {
            // otherwise increment program counter and continue
            frame.pc += 12
          }
          F.pure(Left(frame))
        case Asm.BrTable =>
          // next int gives the number of labels to come
          val nl = frame.readInt()
          // get the label index from stack
          val i = frame.stack.popInt()
          // fix the index to default to the default label
          val idx = if (i >= 0 && i < nl) i else nl
          // retrieve the correct label information at that index
          frame.pc += idx * 12
          val arity = frame.readInt()
          val drop = frame.readInt()
          val addr = frame.readInt()
          br(frame, arity, drop, addr)
          F.pure(Left(frame))
        case Asm.Return =>
          val values = frame.stack.popValues(frame.arity)
          // pop the frame to get the parent
          val frame1 = frame.stack.popFrame()
          if (frame1.isToplevel) {
            // this is the top-level call, return, as we are done
            F.pure(Right(values.headOption))
          } else {
            // push values back to the frame
            frame1.stack.pushValues(values)
            // continue where we left the frame
            F.pure(Left(frame1))
          }
        case Asm.Call =>
          // next integer is the function index
          val fidx = frame.readInt()
          val f = frame.instance.funcs(fidx)
          invoke(frame, f)
        case Asm.CallIndirect =>
          // next integer is the typ index
          val tidx = frame.readInt()
          val tab = frame.instance.tables(0)
          val expectedt = frame.instance.module.types(tidx)
          val i = frame.stack.popInt()
          if (i < 0 || i >= tab.size) {
            F.raiseError(new TrapException(frame, "undefined element"))
          } else if (tab(i) == null) {
            F.raiseError(new TrapException(frame, s"uninitialized element $i"))
          } else {
            val f = tab(i)
            val actualt = f.tpe
            if (expectedt != actualt) {
              F.raiseError(new TrapException(frame, "indirect call type mismatch"))
            } else {
              invoke(frame, f)
            }
          }
        case opcode =>
          F.raiseError(new TrapException(frame, s"unknown opcode 0x${opcode.toHexString}"))
      }
    }

  private def br(frame: Frame[F], arity: Int, drop: Int, addr: Int): Unit = {
    val res = frame.stack.popValues(arity)
    frame.stack.drop(drop)
    frame.stack.pushValues(res)
    frame.pc = addr
  }

  private def invoke(frame: Frame[F], f: Function[F])(
      implicit F: MonadError[F, Throwable]): F[Either[Frame[F], Option[Long]]] =
    f match {
      case FunctionInstance(tpe, locals, code, inst) =>
        val ilocals = Array.ofDim[Long](locals.size + tpe.params.size)
        val zlocals = Array.fill[Long](locals.size)(0l)
        Array.copy(zlocals, 0, ilocals, tpe.params.size, zlocals.length)
        // pop the parameters from the stack
        val params = frame.stack.popValues(tpe.params.size).toArray
        Array.copy(params, 0, ilocals, 0, params.length)
        frame.stack.pushFrame(tpe.t.size, code, ilocals, inst).map(Left(_))
      case _ =>
        // pop the parameters from the stack
        val rawparams = frame.stack.popValues(f.tpe.params.size)
        // convert parameters according to the type defined for function parameters
        val params = f.tpe.params.zip(rawparams).map {
          case (tpe, v) => Value.fromRaw(tpe, v)
        }
        // invoke the host function with the parameters
        f.invoke(params.toVector, frame.instance.memories.headOption).map { res =>
          if (frame.isToplevel) {
            Right(res.map(Value.toRaw(_)))
          } else {
            res.foreach(v => frame.stack.pushValue(Value.toRaw(v)))
            Left(frame)
          }
        }
    }

}
