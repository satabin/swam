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
private[runtime] class Interpreter[F[_]](engine: Engine[F])(implicit F: MonadError[F, Throwable])
    extends interpreter.Interpreter[F](engine) {

  private type Res = Either[Unit, Option[Long]]

  private val conf = engine.conf

  def interpret(funcidx: Int, parameters: Vector[Long], instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level thread
    val thread = new ThreadFrame[F](conf.stack.low, instance)
    // push the parameters in the stack
    thread.pushValues(parameters)
    // invoke the function
    invoke(thread, instance.funcs(funcidx)).flatMap {
      case Left(_)    => run(thread)
      case Right(res) => F.pure(res)
    }
  }

  def interpret(func: Function[F], parameters: Vector[Long], instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level thread
    val thread = new ThreadFrame[F](conf.stack.low, instance)
    // push the parameters in the stack
    thread.pushValues(parameters)
    // invoke the function
    invoke(thread, func).flatMap {
      case Left(_)    => run(thread)
      case Right(res) => F.pure(res)
    }
  }

  def interpretInit(tpe: ValType, code: ByteBuffer, instance: Instance[F]): F[Option[Long]] = {
    // instantiate the top-level thread
    val thread = new ThreadFrame[F](conf.stack.low, instance)
    // invoke the function
    invoke(thread, new FunctionInstance(FuncType(Vector(), Vector(tpe)), Vector(), code, instance)).flatMap {
      case Left(_)    => run(thread)
      case Right(res) => F.pure(res)
    }
  }

  private val continue: F[Either[Unit, Option[Long]]] = F.pure(Left(()))

  private def run(thread: ThreadFrame[F]): F[Option[Long]] =
    F.tailRecM(()) { _ =>
        val opcode = thread.readByte() & 0xff
        (opcode: @switch) match {
          // === constants ===
          case Asm.I32Const =>
            thread.pushInt(thread.readInt())
            continue
          case Asm.I64Const =>
            thread.pushLong(thread.readLong())
            continue
          case Asm.F32Const =>
            thread.pushFloat(thread.readFloat())
            continue
          case Asm.F64Const =>
            thread.pushDouble(thread.readDouble())
            continue
          // === unary operators ===
          case Asm.I32Clz =>
            thread.pushInt(JInt.numberOfLeadingZeros(thread.popInt()))
            continue
          case Asm.I32Ctz =>
            thread.pushInt(JInt.numberOfTrailingZeros(thread.popInt()))
            continue
          case Asm.I32Popcnt =>
            thread.pushInt(JInt.bitCount(thread.popInt()))
            continue
          case Asm.I64Clz =>
            thread.pushLong(JLong.numberOfLeadingZeros(thread.popLong()))
            continue
          case Asm.I64Ctz =>
            thread.pushLong(JLong.numberOfTrailingZeros(thread.popLong()))
            continue
          case Asm.I64Popcnt =>
            thread.pushLong(JLong.bitCount(thread.popLong()))
            continue
          case Asm.F32Abs =>
            thread.pushFloat(JFloat.intBitsToFloat(JFloat.floatToRawIntBits(thread.popFloat()) & 0x7fffffff))
            continue
          case Asm.F32Neg =>
            thread.pushFloat(-thread.popFloat())
            continue
          case Asm.F32Sqrt =>
            thread.pushFloat(StrictMath.sqrt(thread.popFloat()).toFloat)
            continue
          case Asm.F32Ceil =>
            thread.pushFloat(thread.popFloat().ceil)
            continue
          case Asm.F32Floor =>
            thread.pushFloat(thread.popFloat().floor)
            continue
          case Asm.F32Trunc =>
            val f = thread.popFloat()
            thread.pushFloat(F32.trunc(f))
            continue
          case Asm.F32Nearest =>
            val f = thread.popFloat()
            thread.pushFloat(F32.nearest(f))
            continue
          case Asm.F64Abs =>
            thread.pushDouble(
              JDouble.longBitsToDouble(JDouble.doubleToRawLongBits(thread.popDouble()) & 0x7fffffffffffffffl))
            continue
          case Asm.F64Neg =>
            thread.pushDouble(-thread.popDouble())
            continue
          case Asm.F64Sqrt =>
            thread.pushDouble(StrictMath.sqrt(thread.popDouble()))
            continue
          case Asm.F64Ceil =>
            thread.pushDouble(thread.popDouble().ceil)
            continue
          case Asm.F64Floor =>
            thread.pushDouble(thread.popDouble().floor)
            continue
          case Asm.F64Trunc =>
            val f = thread.popDouble()
            thread.pushDouble(F64.trunc(f))
            continue
          case Asm.F64Nearest =>
            val d = thread.popDouble()
            thread.pushDouble(F64.nearest(d))
            continue
          // === binary operators ===
          case Asm.I32Add =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushInt(i1 + i2)
            continue
          case Asm.I32Sub =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushInt(i1 - i2)
            continue
          case Asm.I32Mul =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushInt(i1 * i2)
            continue
          case Asm.I32DivU =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else {
              thread.pushInt(JInt.divideUnsigned(i1, i2))
              continue
            }
          case Asm.I32DivS =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else if (i1 == Int.MinValue && i2 == -1) {
              F.raiseError[Res](new TrapException(thread, "integer overflow"))
            } else {
              val res = i1 / i2
              if (i1 >= 0 && i2 > 0 && res < 0) {
                F.raiseError[Res](new TrapException(thread, "overflow"))
              } else {
                thread.pushInt(i1 / i2)
                continue
              }
            }
          case Asm.I32RemU =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else {
              thread.pushInt(JInt.remainderUnsigned(i1, i2))
              continue
            }
          case Asm.I32RemS =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else {
              thread.pushInt(i1 % i2)
              continue
            }
          case Asm.I32And =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushInt(i1 & i2)
            continue
          case Asm.I32Or =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushInt(i1 | i2)
            continue
          case Asm.I32Xor =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushInt(i1 ^ i2)
            continue
          case Asm.I32Shl =>
            val i2 = thread.popInt() % 32
            val i1 = thread.popInt()
            thread.pushInt(i1 << i2)
            continue
          case Asm.I32ShrU =>
            val i2 = thread.popInt() % 32
            val i1 = thread.popInt()
            thread.pushInt(i1 >>> i2)
            continue
          case Asm.I32ShrS =>
            val i2 = thread.popInt() % 32
            val i1 = thread.popInt()
            thread.pushInt(i1 >> i2)
            continue
          case Asm.I32Rotl =>
            val i2 = thread.popInt() % 32
            val i1 = thread.popInt()
            thread.pushInt(JInt.rotateLeft(i1, i2))
            continue
          case Asm.I32Rotr =>
            val i2 = thread.popInt() % 32
            val i1 = thread.popInt()
            thread.pushInt(JInt.rotateRight(i1, i2))
            continue
          case Asm.I64Add =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushLong(i1 + i2)
            continue
          case Asm.I64Sub =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushLong(i1 - i2)
            continue
          case Asm.I64Mul =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushLong(i1 * i2)
            continue
          case Asm.I64DivU =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else {
              thread.pushLong(JLong.divideUnsigned(i1, i2))
              continue
            }
          case Asm.I64DivS =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else if (i1 == Long.MinValue && i2 == -1l) {
              F.raiseError[Res](new TrapException(thread, "integer overflow"))
            } else {
              val res = i1 / i2
              if (i1 >= 0 && i2 > 0 && res < 0) {
                F.raiseError[Res](new TrapException(thread, "overflow"))
              } else {
                thread.pushLong(i1 / i2)
                continue
              }
            }
          case Asm.I64RemU =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else {
              thread.pushLong(JLong.remainderUnsigned(i1, i2))
              continue
            }
          case Asm.I64RemS =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            if (i2 == 0) {
              F.raiseError[Res](new TrapException(thread, "integer divide by zero"))
            } else {
              thread.pushLong(i1 % i2)
              continue
            }
          case Asm.I64And =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushLong(i1 & i2)
            continue
          case Asm.I64Or =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushLong(i1 | i2)
            continue
          case Asm.I64Xor =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushLong(i1 ^ i2)
            continue
          case Asm.I64Shl =>
            val i2 = thread.popLong() % 64
            val i1 = thread.popLong()
            thread.pushLong(i1 << i2)
            continue
          case Asm.I64ShrU =>
            val i2 = thread.popLong() % 64
            val i1 = thread.popLong()
            thread.pushLong(i1 >>> i2)
            continue
          case Asm.I64ShrS =>
            val i2 = thread.popLong() % 64
            val i1 = thread.popLong()
            thread.pushLong(i1 >> i2)
            continue
          case Asm.I64Rotl =>
            val i2 = (thread.popLong() % 64).toInt
            val i1 = thread.popLong()
            thread.pushLong(JLong.rotateLeft(i1, i2))
            continue
          case Asm.I64Rotr =>
            val i2 = (thread.popLong() % 64).toInt
            val i1 = thread.popLong()
            thread.pushLong(JLong.rotateRight(i1, i2))
            continue
          case Asm.F32Add =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushFloat(f1 + f2)
            continue
          case Asm.F32Sub =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushFloat(f1 - f2)
            continue
          case Asm.F32Mul =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushFloat(f1 * f2)
            continue
          case Asm.F32Div =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushFloat(f1 / f2)
            continue
          case Asm.F32Min =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushFloat(StrictMath.min(f1, f2))
            continue
          case Asm.F32Max =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushFloat(StrictMath.max(f1, f2))
            continue
          case Asm.F32Copysign =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushFloat(Math.copySign(f1, f2))
            continue
          case Asm.F64Add =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushDouble(f1 + f2)
            continue
          case Asm.F64Sub =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushDouble(f1 - f2)
            continue
          case Asm.F64Mul =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushDouble(f1 * f2)
            continue
          case Asm.F64Div =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushDouble(f1 / f2)
            continue
          case Asm.F64Min =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushDouble(StrictMath.min(f1, f2))
            continue
          case Asm.F64Max =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushDouble(StrictMath.max(f1, f2))
            continue
          case Asm.F64Copysign =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushDouble(Math.copySign(f1, f2))
            continue
          // === test operators ===
          case Asm.I32Eqz =>
            val i = thread.popInt()
            thread.pushBool(i == 0)
            continue
          case Asm.I64Eqz =>
            val i = thread.popLong()
            thread.pushBool(i == 0)
            continue
          // === relation operators ===
          case Asm.I32Eq =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(i1 == i2)
            continue
          case Asm.I32Ne =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(i1 != i2)
            continue
          case Asm.I32LtU =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(JInt.compareUnsigned(i1, i2) < 0)
            continue
          case Asm.I32LtS =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(i1 < i2)
            continue
          case Asm.I32GtU =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(JInt.compareUnsigned(i1, i2) > 0)
            continue
          case Asm.I32GtS =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(i1 > i2)
            continue
          case Asm.I32LeU =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(JInt.compareUnsigned(i1, i2) <= 0)
            continue
          case Asm.I32LeS =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(i1 <= i2)
            continue
          case Asm.I32GeU =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(JInt.compareUnsigned(i1, i2) >= 0)
            continue
          case Asm.I32GeS =>
            val i2 = thread.popInt()
            val i1 = thread.popInt()
            thread.pushBool(i1 >= i2)
            continue
          case Asm.I64Eq =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(i1 == i2)
            continue
          case Asm.I64Ne =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(i1 != i2)
            continue
          case Asm.I64LtU =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(JLong.compareUnsigned(i1, i2) < 0)
            continue
          case Asm.I64LtS =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(i1 < i2)
            continue
          case Asm.I64GtU =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(JLong.compareUnsigned(i1, i2) > 0)
            continue
          case Asm.I64GtS =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(i1 > i2)
            continue
          case Asm.I64LeU =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(JLong.compareUnsigned(i1, i2) <= 0)
            continue
          case Asm.I64LeS =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(i1 <= i2)
            continue
          case Asm.I64GeU =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(JLong.compareUnsigned(i1, i2) >= 0)
            continue
          case Asm.I64GeS =>
            val i2 = thread.popLong()
            val i1 = thread.popLong()
            thread.pushBool(i1 >= i2)
            continue
          case Asm.F32Eq =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushBool(f1 == f2)
            continue
          case Asm.F32Ne =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushBool(f1 != f2)
            continue
          case Asm.F32Lt =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushBool(f1 < f2)
            continue
          case Asm.F32Gt =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushBool(f1 > f2)
            continue
          case Asm.F32Le =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushBool(f1 <= f2)
            continue
          case Asm.F32Ge =>
            val f2 = thread.popFloat()
            val f1 = thread.popFloat()
            thread.pushBool(f1 >= f2)
            continue
          case Asm.F64Eq =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushBool(f1 == f2)
            continue
          case Asm.F64Ne =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushBool(f1 != f2)
            continue
          case Asm.F64Lt =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushBool(f1 < f2)
            continue
          case Asm.F64Gt =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushBool(f1 > f2)
            continue
          case Asm.F64Le =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushBool(f1 <= f2)
            continue
          case Asm.F64Ge =>
            val f2 = thread.popDouble()
            val f1 = thread.popDouble()
            thread.pushBool(f1 >= f2)
            continue
          // === conversion operators ===
          case Asm.I32WrapI64 =>
            val l = thread.popLong()
            thread.pushInt(I32.wrap(l))
            continue
          case Asm.I64ExtendUI32 =>
            val i = thread.popInt()
            thread.pushLong(I64.extendUi32(i))
            continue
          case Asm.I64ExtendSI32 =>
            val i = thread.popInt()
            thread.pushLong(I64.extendSi32(i))
            continue
          case Asm.I32TruncUF32 =>
            val f = thread.popFloat()
            I32.truncUf32(f) match {
              case Right(i) =>
                thread.pushInt(i)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.I32TruncSF32 =>
            val f = thread.popFloat()
            I32.truncSf32(f) match {
              case Right(i) =>
                thread.pushInt(i)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.I32TruncUF64 =>
            val f = thread.popDouble()
            I32.truncUf64(f) match {
              case Right(i) =>
                thread.pushInt(i)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.I32TruncSF64 =>
            val f = thread.popDouble()
            I32.truncSf64(f) match {
              case Right(i) =>
                thread.pushInt(i)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.I64TruncUF32 =>
            val f = thread.popFloat()
            I64.truncUf32(f) match {
              case Right(l) =>
                thread.pushLong(l)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.I64TruncSF32 =>
            val f = thread.popFloat()
            I64.truncSf32(f) match {
              case Right(l) =>
                thread.pushLong(l)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.I64TruncUF64 =>
            val f = thread.popDouble()
            I64.truncUf64(f) match {
              case Right(l) =>
                thread.pushLong(l)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.I64TruncSF64 =>
            val f = thread.popDouble()
            I64.truncSf64(f) match {
              case Right(l) =>
                thread.pushLong(l)
                continue
              case Left(msg) =>
                F.raiseError[Res](new TrapException(thread, msg))
            }
          case Asm.F32DemoteF64 =>
            val f = thread.popDouble()
            thread.pushFloat(F32.demote(f))
            continue
          case Asm.F64PromoteF32 =>
            val f = thread.popFloat()
            thread.pushDouble(F64.promote(f))
            continue
          case Asm.F32ConvertUI32 =>
            val i = thread.popInt()
            thread.pushFloat(F32.convertUi32(i))
            continue
          case Asm.F32ConvertSI32 =>
            val i = thread.popInt()
            thread.pushFloat(F32.convertSi32(i))
            continue
          case Asm.F32ConvertUI64 =>
            val l = thread.popLong()
            thread.pushFloat(F32.convertUi64(l))
            continue
          case Asm.F32ConvertSI64 =>
            val l = thread.popLong()
            thread.pushFloat(F32.convertSi64(l))
            continue
          case Asm.F64ConvertUI32 =>
            val i = thread.popInt()
            thread.pushDouble(F64.convertUi32(i))
            continue
          case Asm.F64ConvertSI32 =>
            val i = thread.popInt()
            thread.pushDouble(F64.convertSi32(i))
            continue
          case Asm.F64ConvertUI64 =>
            val l = thread.popLong()
            thread.pushDouble(F64.convertUi64(l))
            continue
          case Asm.F64ConvertSI64 =>
            val l = thread.popLong()
            thread.pushDouble(F64.convertSi64(l))
            continue
          case Asm.I32ReinterpretF32 =>
            val f = thread.popFloat()
            thread.pushInt(I32.reinterpret(f))
            continue
          case Asm.I64ReinterpretF64 =>
            val f = thread.popDouble()
            thread.pushLong(I64.reinterpret(f))
            continue
          case Asm.F32ReinterpretI32 =>
            val i = thread.popInt()
            thread.pushFloat(F32.reinterpret(i))
            continue
          case Asm.F64ReinterpretI64 =>
            val i = thread.popLong()
            thread.pushDouble(F64.reinterpret(i))
            continue
          // === parameteric instructions ===
          case Asm.Drop =>
            val n = thread.readInt()
            thread.drop(n)
            continue
          case Asm.Select =>
            val b = thread.popBool()
            val v2 = thread.popValue()
            val v1 = thread.popValue()
            if (b)
              thread.pushValue(v1)
            else
              thread.pushValue(v2)
            continue
          case Asm.LocalGet =>
            val idx = thread.readInt()
            thread.pushValue(thread.local(idx))
            continue
          case Asm.LocalSet =>
            val idx = thread.readInt()
            val v = thread.popValue()
            thread.setLocal(idx, v)
            continue
          case Asm.LocalTee =>
            val idx = thread.readInt()
            val v = thread.peekValue()
            thread.setLocal(idx, v)
            continue
          case Asm.GlobalGet =>
            val idx = thread.readInt()
            thread.global(idx) match {
              case i: GlobalInstance[F] =>
                thread.pushValue(i.rawget)
              case g =>
                thread.pushValue(Value.toRaw(g.get))
            }
            continue
          case Asm.GlobalSet =>
            val idx = thread.readInt()
            val v = thread.popValue()
            thread.global(idx) match {
              case i: GlobalInstance[F] =>
                i.rawset(v)
                continue
              case g =>
                g.set(Value.fromRaw(g.tpe.tpe, v)) >> continue
            }
          // === memory instructions ===
          case Asm.I32Load =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 4 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readInt(ea).flatMap { c =>
                thread.pushInt(c)
                continue
              }
          case Asm.I32Load8U =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 1 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readByte(ea).flatMap { c =>
                thread.pushInt(c & 0xff)
                continue
              }
          case Asm.I32Load8S =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 1 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readByte(ea).flatMap { c =>
                thread.pushInt(c)
                continue
              }
          case Asm.I32Load16U =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 2 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readShort(ea).flatMap { c =>
                thread.pushInt(c & 0xffff)
                continue
              }
          case Asm.I32Load16S =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 2 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readShort(ea).flatMap { c =>
                thread.pushInt(c)
                continue
              }
          case Asm.I64Load =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 8 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readLong(ea).flatMap { c =>
                thread.pushLong(c)
                continue
              }
          case Asm.I64Load8U =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 1 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readByte(ea).flatMap { c =>
                thread.pushLong(c & 0xffl)
                continue
              }
          case Asm.I64Load8S =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 1 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readByte(ea).flatMap { c =>
                thread.pushLong(c)
                continue
              }
          case Asm.I64Load16U =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 2 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readShort(ea).flatMap { c =>
                thread.pushLong(c & 0xffffl)
                continue
              }
          case Asm.I64Load16S =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 2 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readShort(ea).flatMap { c =>
                thread.pushLong(c)
                continue
              }
          case Asm.I64Load32U =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 4 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readInt(ea).flatMap { c =>
                thread.pushLong(c & 0xffffffffl)
                continue
              }
          case Asm.I64Load32S =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 4 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readInt(ea).flatMap { c =>
                thread.pushLong(c)
                continue
              }
          case Asm.F32Load =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 4 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readFloat(ea).flatMap { c =>
                thread.pushFloat(c)
                continue
              }
          case Asm.F64Load =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 8 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.readDouble(ea).flatMap { c =>
                thread.pushDouble(c)
                continue
              }
          case Asm.I32Store =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popInt()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 4 > mem.size)
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            else
              mem.writeInt(ea, c) >> continue
          case Asm.I32Store8 =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popInt()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              val c1 = (c % (1 << 8)).toByte
              mem.writeByte(ea, c1) >> continue
            }
          case Asm.I32Store16 =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popInt()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              val c1 = (c % (1 << 16)).toShort
              mem.writeShort(ea, c1) >> continue
            }
          case Asm.I64Store =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popLong()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              mem.writeLong(ea, c) >> continue
            }
          case Asm.I64Store8 =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popLong()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              val c1 = (c % (1l << 8)).toByte
              mem.writeByte(ea, c1) >> continue
            }
          case Asm.I64Store16 =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popLong()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              val c1 = (c % (1l << 16)).toShort
              mem.writeShort(ea, c1) >> continue
            }
          case Asm.I64Store32 =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popLong()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              val c1 = (c % (1l << 32)).toInt
              mem.writeInt(ea, c1) >> continue
            }
          case Asm.F32Store =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popFloat()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              mem.writeFloat(ea, c) >> continue
            }
          case Asm.F64Store =>
            val align = thread.readInt()
            val offset = thread.readInt()
            val mem = thread.memory(0)
            val c = thread.popDouble()
            val i = thread.popInt()
            val ea = i + offset
            if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
              F.raiseError[Res](new TrapException(thread, "out of bounds memory access"))
            } else {
              mem.writeDouble(ea, c) >> continue
            }
          case Asm.MemorySize =>
            val mem = thread.memory(0)
            val sz = mem.size / pageSize
            thread.pushInt(sz)
            continue
          case Asm.MemoryGrow =>
            val mem = thread.memory(0)
            val sz = mem.size / pageSize
            val n = thread.popInt()
            mem
              .grow(n)
              .map {
                case true  => thread.pushInt(sz)
                case flase => thread.pushInt(-1)
              } >> continue
          // === control instructions ===
          case Asm.Nop =>
            continue
          case Asm.Unreachable =>
            F.raiseError[Res](new TrapException(thread, "unreachable executed"))
          case Asm.Jump =>
            // next comes the jump address
            val addr = thread.readInt()
            thread.pc = addr
            continue
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
              // fix the pc and continue
              thread.pc += 4
            }
            continue
          case Asm.Br =>
            // next comes the label arity
            val arity = thread.readInt()
            // then the rest to drop
            val drop = thread.readInt()
            // and finally the jump address
            val addr = thread.readInt()
            br(thread, arity, drop, addr)
            continue
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
              // otherwise increment program counter and continue
              thread.pc += 12
            }
            continue
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
            continue
          case Asm.Return =>
            val values = thread.popValues(thread.arity)
            // pop the thread to get the parent
            thread.popFrame()
            if (thread.isToplevel) {
              // this is the top-level call, return, as we are done
              F.pure(values.headOption.asRight[Unit])
            } else {
              // push values back to the thread
              thread.pushValues(values)
              // continue where we left the thread
              continue
            }
          case Asm.Call =>
            // next integer is the function index
            val fidx = thread.readInt()
            val f = thread.func(fidx)
            invoke(thread, f)
          case Asm.CallIndirect =>
            // next integer is the typ index
            val tidx = thread.readInt()
            val tab = thread.table(0)
            val expectedt = thread.module.types(tidx)
            val i = thread.popInt()
            if (i < 0 || i >= tab.size) {
              F.raiseError[Res](new TrapException(thread, "undefined element"))
            } else if (tab(i) == null) {
              F.raiseError[Res](new TrapException(thread, s"uninitialized element $i"))
            } else {
              val f = tab(i)
              val actualt = f.tpe
              if (expectedt != actualt) {
                F.raiseError[Res](new TrapException(thread, "indirect call type mismatch"))
              } else {
                invoke(thread, f)
              }
            }
          case opcode =>
            F.raiseError[Res](new TrapException(thread, s"unknown opcode 0x${opcode.toHexString}"))
        }
      }
      .adaptError {
        case e: ArrayIndexOutOfBoundsException => new StackOverflowException(thread, e)
      }

  private def br(thread: ThreadFrame[F], arity: Int, drop: Int, addr: Int): Unit = {
    val res = thread.popValues(arity)
    thread.drop(drop)
    thread.pushValues(res)
    thread.pc = addr
  }

  private def invoke(thread: ThreadFrame[F], f: Function[F])(
      implicit F: MonadError[F, Throwable]): F[Either[Unit, Option[Long]]] =
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
        f.invoke(params.toVector, thread.memoryOpt(0)).flatMap { res =>
          if (thread.isToplevel) {
            F.pure(Right(res.map(Value.toRaw(_))))
          } else {
            res.foreach(v => thread.pushValue(Value.toRaw(v)))
            continue
          }
        }
    }

}
