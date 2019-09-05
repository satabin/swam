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

import swam.{syntax => sy}
import instance._

import cats._
import cats.implicits._

import java.lang.{Integer => JInt, Long => JLong, Float => JFloat, Double => JDouble}

/** `Asm` is the interpreted language. It closely mirrors the WebAssembly bytecode with
  *  few differences:
  *   - it has typed breaks;
  *   - it has no blocks, loops, or if structures, they are all compiled to breaks;
  *   - it has a parameterized drop operation to drop several elements from the stack at once;
  *   - it has arbitrary (un)conditional jump instructions.
  *
  * This results in a language where labels are pre-compiled, avoiding indirections,
  * and avoiding managing them on the stack.
  * The assembly language is also less safe as it has arbitrary jumps inside a method body.
  * The bytecode compiler is responsible for
  * generating correctly typed assembly code from the bytecode.
  *
  * Breaks are parameterized by the jump addresses, the number of return values and the number
  * of elements to discard from the stack before pushing return values back.
  */
sealed trait AsmInst[F[_]] {
  def execute(t: ThreadFrame[F]): Continuation[F]
}

sealed trait Continuation[+F[_]]
case object Continue extends Continuation[Nothing]
case class Suspend[F[_]](res: F[Option[Long]]) extends Continuation[F]
case class Done(res: Option[Long]) extends Continuation[Nothing]

class Asm[F[_]](implicit F: MonadError[F, Throwable]) {

  object Unop {
    def apply(unop: sy.Unop): AsmInst[F] =
      unop match {
        case sy.i32.Clz     => I32Clz
        case sy.i32.Ctz     => I32Ctz
        case sy.i32.Popcnt  => I32Popcnt
        case sy.i64.Clz     => I64Clz
        case sy.i64.Ctz     => I64Ctz
        case sy.i64.Popcnt  => I64Popcnt
        case sy.f32.Abs     => F32Abs
        case sy.f32.Neg     => F32Neg
        case sy.f32.Sqrt    => F32Sqrt
        case sy.f32.Ceil    => F32Ceil
        case sy.f32.Floor   => F32Floor
        case sy.f32.Trunc   => F32Trunc
        case sy.f32.Nearest => F32Nearest
        case sy.f64.Abs     => F64Abs
        case sy.f64.Neg     => F64Neg
        case sy.f64.Sqrt    => F64Sqrt
        case sy.f64.Ceil    => F64Ceil
        case sy.f64.Floor   => F64Floor
        case sy.f64.Trunc   => F64Trunc
        case sy.f64.Nearest => F64Nearest
      }
  }

  object Binop {
    def apply(binop: sy.Binop): AsmInst[F] =
      binop match {
        case sy.i32.Add      => I32Add
        case sy.i32.Sub      => I32Sub
        case sy.i32.Mul      => I32Mul
        case sy.i32.DivS     => I32DivS
        case sy.i32.DivU     => I32DivU
        case sy.i32.RemS     => I32RemS
        case sy.i32.RemU     => I32RemU
        case sy.i32.And      => I32And
        case sy.i32.Or       => I32Or
        case sy.i32.Xor      => I32Xor
        case sy.i32.Shl      => I32Shl
        case sy.i32.ShrS     => I32ShrS
        case sy.i32.ShrU     => I32ShrU
        case sy.i32.Rotl     => I32Rotl
        case sy.i32.Rotr     => I32Rotr
        case sy.i64.Add      => I64Add
        case sy.i64.Sub      => I64Sub
        case sy.i64.Mul      => I64Mul
        case sy.i64.DivS     => I64DivS
        case sy.i64.DivU     => I64DivU
        case sy.i64.RemS     => I64RemS
        case sy.i64.RemU     => I64RemU
        case sy.i64.And      => I64And
        case sy.i64.Or       => I64Or
        case sy.i64.Xor      => I64Xor
        case sy.i64.Shl      => I64Shl
        case sy.i64.ShrS     => I64ShrS
        case sy.i64.ShrU     => I64ShrU
        case sy.i64.Rotl     => I64Rotl
        case sy.i64.Rotr     => I64Rotr
        case sy.f32.Add      => F32Add
        case sy.f32.Sub      => F32Sub
        case sy.f32.Mul      => F32Mul
        case sy.f32.Div      => F32Div
        case sy.f32.Min      => F32Min
        case sy.f32.Max      => F32Max
        case sy.f32.Copysign => F32Copysign
        case sy.f64.Add      => F64Add
        case sy.f64.Sub      => F64Sub
        case sy.f64.Mul      => F64Mul
        case sy.f64.Div      => F64Div
        case sy.f64.Min      => F64Min
        case sy.f64.Max      => F64Max
        case sy.f64.Copysign => F64Copysign
      }
  }

  object Testop {
    def apply(testop: sy.Testop): AsmInst[F] =
      testop match {
        case sy.i32.Eqz => I32Eqz
        case sy.i64.Eqz => I64Eqz
      }
  }

  object Relop {
    def apply(relop: sy.Relop): AsmInst[F] =
      relop match {
        case sy.i32.Eq  => I32Eq
        case sy.i32.Ne  => I32Ne
        case sy.i32.LtS => I32LtS
        case sy.i32.LtU => I32LtU
        case sy.i32.GtS => I32GtS
        case sy.i32.GtU => I32GtU
        case sy.i32.LeS => I32LeS
        case sy.i32.LeU => I32LeU
        case sy.i32.GeS => I32GeS
        case sy.i32.GeU => I32GeU
        case sy.i64.Eq  => I64Eq
        case sy.i64.Ne  => I64Ne
        case sy.i64.LtS => I64LtS
        case sy.i64.LtU => I64LtU
        case sy.i64.GtS => I64GtS
        case sy.i64.GtU => I64GtU
        case sy.i64.LeS => I64LeS
        case sy.i64.LeU => I64LeU
        case sy.i64.GeS => I64GeS
        case sy.i64.GeU => I64GeU
        case sy.f32.Eq  => F32Eq
        case sy.f32.Ne  => F32Ne
        case sy.f32.Lt  => F32Lt
        case sy.f32.Gt  => F32Gt
        case sy.f32.Le  => F32Le
        case sy.f32.Ge  => F32Ge
        case sy.f64.Eq  => F64Eq
        case sy.f64.Ne  => F64Ne
        case sy.f64.Lt  => F64Lt
        case sy.f64.Gt  => F64Gt
        case sy.f64.Le  => F64Le
        case sy.f64.Ge  => F64Ge
      }
  }

  object Convertop {
    def apply(convertop: sy.Convertop): AsmInst[F] =
      convertop match {
        case sy.i32.WrapI64        => I32WrapI64
        case sy.i32.TruncSF32      => I32TruncSF32
        case sy.i32.TruncUF32      => I32TruncUF32
        case sy.i32.TruncSF64      => I32TruncSF64
        case sy.i32.TruncUF64      => I32TruncUF64
        case sy.i32.ReinterpretF32 => I32ReinterpretF32
        case sy.i64.ExtendSI32     => I64ExtendSI32
        case sy.i64.ExtendUI32     => I64ExtendUI32
        case sy.i64.TruncSF32      => I64TruncSF32
        case sy.i64.TruncUF32      => I64TruncUF32
        case sy.i64.TruncSF64      => I64TruncSF64
        case sy.i64.TruncUF64      => I64TruncUF64
        case sy.i64.ReinterpretF64 => I64ReinterpretF64
        case sy.f32.DemoteF64      => F32DemoteF64
        case sy.f32.ConvertSI32    => F32ConvertSI32
        case sy.f32.ConvertUI32    => F32ConvertUI32
        case sy.f32.ConvertSI64    => F32ConvertSI64
        case sy.f32.ConvertUI64    => F32ConvertUI64
        case sy.f32.ReinterpretI32 => F32ReinterpretI32
        case sy.f64.PromoteF32     => F64PromoteF32
        case sy.f64.ConvertSI32    => F64ConvertSI32
        case sy.f64.ConvertUI32    => F64ConvertUI32
        case sy.f64.ConvertSI64    => F64ConvertSI64
        case sy.f64.ConvertUI64    => F64ConvertUI64
        case sy.f64.ReinterpretI64 => F64ReinterpretI64
      }
  }

  object Load {
    def apply(load: sy.LoadInst): AsmInst[F] =
      load match {
        case sy.i32.Load(align, offset) => new I32Load(align, offset)
        case sy.i64.Load(align, offset) => new I64Load(align, offset)
        case sy.f32.Load(align, offset) => new F32Load(align, offset)
        case sy.f64.Load(align, offset) => new F64Load(align, offset)
      }
  }

  object LoadN {
    def apply(loadn: sy.LoadNInst): AsmInst[F] =
      loadn match {
        case sy.i32.Load8S(align, offset)  => new I32Load8S(align, offset)
        case sy.i32.Load8U(align, offset)  => new I32Load8U(align, offset)
        case sy.i32.Load16S(align, offset) => new I32Load16S(align, offset)
        case sy.i32.Load16U(align, offset) => new I32Load16U(align, offset)
        case sy.i64.Load8S(align, offset)  => new I64Load8S(align, offset)
        case sy.i64.Load8U(align, offset)  => new I64Load8U(align, offset)
        case sy.i64.Load16S(align, offset) => new I64Load16S(align, offset)
        case sy.i64.Load16U(align, offset) => new I64Load16U(align, offset)
        case sy.i64.Load32S(align, offset) => new I64Load32S(align, offset)
        case sy.i64.Load32U(align, offset) => new I64Load32U(align, offset)
      }
  }

  object Store {
    def apply(store: sy.StoreInst): AsmInst[F] =
      store match {
        case sy.i32.Store(align, offset) => new I32Store(align, offset)
        case sy.i64.Store(align, offset) => new I64Store(align, offset)
        case sy.f32.Store(align, offset) => new F32Store(align, offset)
        case sy.f64.Store(align, offset) => new F64Store(align, offset)
      }
  }

  object StoreN {
    def apply(storen: sy.StoreNInst): AsmInst[F] =
      storen match {
        case sy.i32.Store8(align, offset)  => new I32Store8(align, offset)
        case sy.i32.Store16(align, offset) => new I32Store16(align, offset)
        case sy.i64.Store8(align, offset)  => new I64Store8(align, offset)
        case sy.i64.Store16(align, offset) => new I64Store16(align, offset)
        case sy.i64.Store32(align, offset) => new I64Store32(align, offset)
      }
  }

  class I32Const(v: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushInt(v)
      Continue
    }
  }

  class I64Const(v: Long) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushLong(v)
      Continue
    }
  }

  class F32Const(v: Float) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushFloat(v)
      Continue
    }
  }

  class F64Const(v: Double) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushDouble(v)
      Continue
    }
  }

  case object I32Clz extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushInt(JInt.numberOfLeadingZeros(thread.popInt()))
      Continue
    }
  }

  case object I32Ctz extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushInt(JInt.numberOfTrailingZeros(thread.popInt()))
      Continue
    }
  }

  case object I32Popcnt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushInt(JInt.bitCount(thread.popInt()))
      Continue
    }
  }

  case object I64Clz extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushLong(JLong.numberOfLeadingZeros(thread.popLong()))
      Continue
    }
  }

  case object I64Ctz extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushLong(JLong.numberOfTrailingZeros(thread.popLong()))
      Continue
    }
  }

  case object I64Popcnt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushLong(JLong.bitCount(thread.popLong()))
      Continue
    }
  }

  case object F32Abs extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushFloat(JFloat.intBitsToFloat(JFloat.floatToRawIntBits(thread.popFloat()) & 0x7fffffff))
      Continue
    }
  }

  case object F32Neg extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushFloat(-thread.popFloat())
      Continue
    }
  }

  case object F32Sqrt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushFloat(StrictMath.sqrt(thread.popFloat()).toFloat)
      Continue
    }
  }

  case object F32Ceil extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushFloat(thread.popFloat().ceil)
      Continue
    }
  }

  case object F32Floor extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushFloat(thread.popFloat().floor)
      Continue
    }
  }

  case object F32Trunc extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      thread.pushFloat(F32.trunc(f))
      Continue
    }
  }

  case object F32Nearest extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      thread.pushFloat(F32.nearest(f))
      Continue
    }
  }

  case object F64Abs extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushDouble(JDouble.longBitsToDouble(JDouble.doubleToRawLongBits(thread.popDouble()) & 0X7FFFFFFFFFFFFFFFL))
      Continue
    }
  }

  case object F64Neg extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushDouble(-thread.popDouble())
      Continue
    }
  }

  case object F64Sqrt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushDouble(StrictMath.sqrt(thread.popDouble()))
      Continue
    }
  }

  case object F64Ceil extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushDouble(thread.popDouble().ceil)
      Continue
    }
  }

  case object F64Floor extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushDouble(thread.popDouble().floor)
      Continue
    }
  }

  case object F64Trunc extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popDouble()
      thread.pushDouble(F64.trunc(f))
      Continue
    }
  }

  case object F64Nearest extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val d = thread.popDouble()
      thread.pushDouble(F64.nearest(d))
      Continue
    }
  }

  case object I32Add extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushInt(i1 + i2)
      Continue
    }
  }

  case object I32Sub extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushInt(i1 - i2)
      Continue
    }
  }

  case object I32Mul extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushInt(i1 * i2)
      Continue
    }
  }

  case object I32DivU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else {
        thread.pushInt(JInt.divideUnsigned(i1, i2))
        Continue
      }
    }
  }
  case object I32DivS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else if (i1 == Int.MinValue && i2 == -1) {
        throw new TrapException(thread, "integer overflow")
      } else {
        val res = i1 / i2
        if (i1 >= 0 && i2 > 0 && res < 0) {
          throw new TrapException(thread, "overflow")
        } else {
          thread.pushInt(res)
          Continue
        }
      }
    }
  }
  case object I32RemU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else {
        thread.pushInt(JInt.remainderUnsigned(i1, i2))
        Continue
      }
    }
  }
  case object I32RemS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else {
        thread.pushInt(i1 % i2)
        Continue
      }
    }
  }
  case object I32And extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushInt(i1 & i2)
      Continue
    }
  }

  case object I32Or extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushInt(i1 | i2)
      Continue
    }
  }

  case object I32Xor extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushInt(i1 ^ i2)
      Continue
    }
  }

  case object I32Shl extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt() % 32
      val i1 = thread.popInt()
      thread.pushInt(i1 << i2)
      Continue
    }
  }

  case object I32ShrU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt() % 32
      val i1 = thread.popInt()
      thread.pushInt(i1 >>> i2)
      Continue
    }
  }

  case object I32ShrS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt() % 32
      val i1 = thread.popInt()
      thread.pushInt(i1 >> i2)
      Continue
    }
  }

  case object I32Rotl extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt() % 32
      val i1 = thread.popInt()
      thread.pushInt(JInt.rotateLeft(i1, i2))
      Continue
    }
  }

  case object I32Rotr extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt() % 32
      val i1 = thread.popInt()
      thread.pushInt(JInt.rotateRight(i1, i2))
      Continue
    }
  }

  case object I64Add extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushLong(i1 + i2)
      Continue
    }
  }

  case object I64Sub extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushLong(i1 - i2)
      Continue
    }
  }

  case object I64Mul extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushLong(i1 * i2)
      Continue
    }
  }

  case object I64DivU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else {
        thread.pushLong(JLong.divideUnsigned(i1, i2))
        Continue
      }
    }
  }

  case object I64DivS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else if (i1 == Long.MinValue && i2 == -1L) {
        throw new TrapException(thread, "integer overflow")
      } else {
        val res = i1 / i2
        if (i1 >= 0 && i2 > 0 && res < 0) {
          throw new TrapException(thread, "overflow")
        } else {
          thread.pushLong(i1 / i2)
          Continue

        }
      }
    }
  }

  case object I64RemU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else {
        thread.pushLong(JLong.remainderUnsigned(i1, i2))
        Continue
      }
    }
  }

  case object I64RemS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      if (i2 == 0) {
        throw new TrapException(thread, "integer divide by zero")
      } else {
        thread.pushLong(i1 % i2)
        Continue
      }
    }
  }

  case object I64And extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushLong(i1 & i2)
      Continue
    }
  }

  case object I64Or extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushLong(i1 | i2)
      Continue
    }
  }

  case object I64Xor extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushLong(i1 ^ i2)
      Continue
    }
  }

  case object I64Shl extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong() % 64
      val i1 = thread.popLong()
      thread.pushLong(i1 << i2)
      Continue
    }
  }

  case object I64ShrU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong() % 64
      val i1 = thread.popLong()
      thread.pushLong(i1 >>> i2)
      Continue
    }
  }

  case object I64ShrS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong() % 64
      val i1 = thread.popLong()
      thread.pushLong(i1 >> i2)
      Continue
    }
  }

  case object I64Rotl extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = (thread.popLong() % 64).toInt
      val i1 = thread.popLong()
      thread.pushLong(JLong.rotateLeft(i1, i2))
      Continue
    }
  }

  case object I64Rotr extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = (thread.popLong() % 64).toInt
      val i1 = thread.popLong()
      thread.pushLong(JLong.rotateRight(i1, i2))
      Continue
    }
  }

  case object F32Add extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushFloat(f1 + f2)
      Continue
    }
  }

  case object F32Sub extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushFloat(f1 - f2)
      Continue
    }
  }

  case object F32Mul extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushFloat(f1 * f2)
      Continue
    }
  }

  case object F32Div extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushFloat(f1 / f2)
      Continue
    }
  }

  case object F32Min extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushFloat(StrictMath.min(f1, f2))
      Continue
    }
  }

  case object F32Max extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushFloat(StrictMath.max(f1, f2))
      Continue
    }
  }

  case object F32Copysign extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushFloat(Math.copySign(f1, f2))
      Continue
    }
  }

  case object F64Add extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushDouble(f1 + f2)
      Continue
    }
  }

  case object F64Sub extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushDouble(f1 - f2)
      Continue
    }
  }

  case object F64Mul extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushDouble(f1 * f2)
      Continue
    }
  }

  case object F64Div extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushDouble(f1 / f2)
      Continue
    }
  }

  case object F64Min extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushDouble(StrictMath.min(f1, f2))
      Continue
    }
  }

  case object F64Max extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushDouble(StrictMath.max(f1, f2))
      Continue
    }
  }

  case object F64Copysign extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushDouble(Math.copySign(f1, f2))
      Continue
    }
  }

  case object I32Eqz extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushBool(i == 0)
      Continue
    }
  }

  case object I64Eqz extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popLong()
      thread.pushBool(i == 0)
      Continue
    }
  }

  case object I32Eq extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(i1 == i2)
      Continue
    }
  }

  case object I32Ne extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(i1 != i2)
      Continue
    }
  }

  case object I32LtU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(JInt.compareUnsigned(i1, i2) < 0)
      Continue
    }
  }

  case object I32LtS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(i1 < i2)
      Continue
    }
  }

  case object I32GtU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(JInt.compareUnsigned(i1, i2) > 0)
      Continue
    }
  }

  case object I32GtS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(i1 > i2)
      Continue
    }
  }

  case object I32LeU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(JInt.compareUnsigned(i1, i2) <= 0)
      Continue
    }
  }

  case object I32LeS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(i1 <= i2)
      Continue
    }
  }

  case object I32GeU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(JInt.compareUnsigned(i1, i2) >= 0)
      Continue
    }
  }

  case object I32GeS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popInt()
      val i1 = thread.popInt()
      thread.pushBool(i1 >= i2)
      Continue
    }
  }

  case object I64Eq extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(i1 == i2)
      Continue
    }
  }

  case object I64Ne extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(i1 != i2)
      Continue
    }
  }

  case object I64LtU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(JLong.compareUnsigned(i1, i2) < 0)
      Continue
    }
  }

  case object I64LtS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(i1 < i2)
      Continue
    }
  }

  case object I64GtU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(JLong.compareUnsigned(i1, i2) > 0)
      Continue
    }
  }

  case object I64GtS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(i1 > i2)
      Continue
    }
  }

  case object I64LeU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(JLong.compareUnsigned(i1, i2) <= 0)
      Continue
    }
  }

  case object I64LeS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(i1 <= i2)
      Continue
    }
  }

  case object I64GeU extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(JLong.compareUnsigned(i1, i2) >= 0)
      Continue
    }
  }

  case object I64GeS extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i2 = thread.popLong()
      val i1 = thread.popLong()
      thread.pushBool(i1 >= i2)
      Continue
    }
  }

  case object F32Eq extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushBool(f1 == f2)
      Continue
    }
  }

  case object F32Ne extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushBool(f1 != f2)
      Continue
    }
  }

  case object F32Lt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushBool(f1 < f2)
      Continue
    }
  }

  case object F32Gt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushBool(f1 > f2)
      Continue
    }
  }

  case object F32Le extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushBool(f1 <= f2)
      Continue
    }
  }

  case object F32Ge extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popFloat()
      val f1 = thread.popFloat()
      thread.pushBool(f1 >= f2)
      Continue
    }
  }

  case object F64Eq extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushBool(f1 == f2)
      Continue
    }
  }

  case object F64Ne extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushBool(f1 != f2)
      Continue
    }
  }

  case object F64Lt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushBool(f1 < f2)
      Continue
    }
  }

  case object F64Gt extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushBool(f1 > f2)
      Continue
    }
  }

  case object F64Le extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushBool(f1 <= f2)
      Continue
    }
  }

  case object F64Ge extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f2 = thread.popDouble()
      val f1 = thread.popDouble()
      thread.pushBool(f1 >= f2)
      Continue
    }
  }

  case object I32WrapI64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val l = thread.popLong()
      thread.pushInt(I32.wrap(l))
      Continue
    }
  }

  case object I64ExtendUI32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushLong(I64.extendUi32(i))
      Continue
    }
  }

  case object I64ExtendSI32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushLong(I64.extendSi32(i))
      Continue
    }
  }

  case object I32TruncUF32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      I32.truncUf32(f) match {
        case Right(i) =>
          thread.pushInt(i)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object I32TruncSF32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      I32.truncSf32(f) match {
        case Right(i) =>
          thread.pushInt(i)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object I32TruncUF64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popDouble()
      I32.truncUf64(f) match {
        case Right(i) =>
          thread.pushInt(i)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object I32TruncSF64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popDouble()
      I32.truncSf64(f) match {
        case Right(i) =>
          thread.pushInt(i)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object I64TruncUF32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      I64.truncUf32(f) match {
        case Right(l) =>
          thread.pushLong(l)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object I64TruncSF32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      I64.truncSf32(f) match {
        case Right(l) =>
          thread.pushLong(l)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object I64TruncUF64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popDouble()
      I64.truncUf64(f) match {
        case Right(l) =>
          thread.pushLong(l)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object I64TruncSF64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popDouble()
      I64.truncSf64(f) match {
        case Right(l) =>
          thread.pushLong(l)
          Continue
        case Left(msg) =>
          throw new TrapException(thread, msg)
      }
    }
  }

  case object F32DemoteF64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popDouble()
      thread.pushFloat(F32.demote(f))
      Continue
    }
  }

  case object F64PromoteF32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      thread.pushDouble(F64.promote(f))
      Continue
    }
  }

  case object F32ConvertUI32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushFloat(F32.convertUi32(i))
      Continue
    }
  }

  case object F32ConvertSI32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushFloat(F32.convertSi32(i))
      Continue
    }
  }

  case object F32ConvertUI64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val l = thread.popLong()
      thread.pushFloat(F32.convertUi64(l))
      Continue
    }
  }

  case object F32ConvertSI64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val l = thread.popLong()
      thread.pushFloat(F32.convertSi64(l))
      Continue
    }
  }

  case object F64ConvertUI32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushDouble(F64.convertUi32(i))
      Continue
    }
  }

  case object F64ConvertSI32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushDouble(F64.convertSi32(i))
      Continue
    }
  }

  case object F64ConvertUI64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val l = thread.popLong()
      thread.pushDouble(F64.convertUi64(l))
      Continue
    }
  }

  case object F64ConvertSI64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val l = thread.popLong()
      thread.pushDouble(F64.convertSi64(l))
      Continue
    }
  }

  case object I32ReinterpretF32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popFloat()
      thread.pushInt(I32.reinterpret(f))
      Continue
    }
  }

  case object I64ReinterpretF64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.popDouble()
      thread.pushLong(I64.reinterpret(f))
      Continue
    }
  }

  case object F32ReinterpretI32 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popInt()
      thread.pushFloat(F32.reinterpret(i))
      Continue
    }
  }

  case object F64ReinterpretI64 extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val i = thread.popLong()
      thread.pushDouble(F64.reinterpret(i))
      Continue
    }
  }

  class Drop(n: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.drop(n)
      Continue
    }
  }

  case object Select extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val b = thread.popBool()
      val v2 = thread.popValue()
      val v1 = thread.popValue()
      if (b)
        thread.pushValue(v1)
      else
        thread.pushValue(v2)
      Continue
    }
  }

  class LocalGet(idx: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.pushValue(thread.local(idx))
      Continue
    }
  }

  class LocalSet(idx: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val v = thread.popValue()
      thread.setLocal(idx, v)
      Continue
    }
  }

  class LocalTee(idx: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val v = thread.peekValue()
      thread.setLocal(idx, v)
      Continue
    }
  }

  class GlobalGet(idx: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.global(idx) match {
        case i: GlobalInstance[F] =>
          thread.pushValue(i.rawget)
        case g =>
          thread.pushValue(Value.toRaw(g.get))
      }
      Continue
    }
  }

  class GlobalSet(idx: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val v = thread.popValue()
      thread.global(idx) match {
        case i: GlobalInstance[F] =>
          i.rawset(v)
        case g =>
          g.set(Value.fromRaw(g.tpe.tpe, v))
      }
      Continue
    }
  }

  class I32Load(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadInt(ea)
        thread.pushInt(c)
        Continue

      }
    }
  }

  class I32Load8U(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadByte(ea)
        thread.pushInt(c & 0xff)
        Continue
      }
    }
  }

  class I32Load8S(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadByte(ea)
        thread.pushInt(c)
        Continue
      }
    }
  }

  class I32Load16U(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadShort(ea)
        thread.pushInt(c & 0xffff)
        Continue
      }
    }
  }

  class I32Load16S(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadShort(ea)
        thread.pushInt(c)
        Continue
      }
    }
  }

  class I64Load(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadLong(ea)
        thread.pushLong(c)
        Continue
      }
    }
  }

  class I64Load8U(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadByte(ea)
        thread.pushLong(c & 0XFFL)
        Continue
      }
    }
  }

  class I64Load8S(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadByte(ea)
        thread.pushLong(c)
        Continue
      }
    }
  }

  class I64Load16U(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadShort(ea)
        thread.pushLong(c & 0XFFFFL)
        Continue
      }
    }
  }

  class I64Load16S(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadShort(ea)
        thread.pushLong(c)
        Continue
      }
    }
  }

  class I64Load32U(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadInt(ea)
        thread.pushLong(c & 0XFFFFFFFFL)
        Continue
      }
    }
  }

  class I64Load32S(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadInt(ea)
        thread.pushLong(c)
        Continue
      }
    }
  }

  class F32Load(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadFloat(ea)
        thread.pushFloat(c)
        Continue
      }
    }
  }

  class F64Load(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c = mem.unsafeReadDouble(ea)
        thread.pushDouble(c)
        Continue
      }
    }
  }

  class I32Store(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popInt()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        mem.unsafeWriteInt(ea, c)
        Continue
      }
    }
  }

  class I32Store8(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popInt()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c1 = (c % (1 << 8)).toByte
        mem.unsafeWriteByte(ea, c1)
        Continue
      }
    }
  }

  class I32Store16(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popInt()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c1 = (c % (1 << 16)).toShort
        mem.unsafeWriteShort(ea, c1)
        Continue
      }
    }
  }

  class I64Store(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popLong()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        mem.unsafeWriteLong(ea, c)
        Continue
      }
    }
  }

  class I64Store8(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popLong()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c1 = (c % (1L << 8)).toByte
        mem.unsafeWriteByte(ea, c1)
        Continue
      }
    }
  }

  class I64Store16(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popLong()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c1 = (c % (1L << 16)).toShort
        mem.unsafeWriteShort(ea, c1)
        Continue
      }
    }
  }

  class I64Store32(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popLong()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        val c1 = (c % (1L << 32)).toInt
        mem.unsafeWriteInt(ea, c1)
        Continue
      }
    }
  }

  class F32Store(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popFloat()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        mem.unsafeWriteFloat(ea, c)
        Continue
      }
    }
  }

  class F64Store(alignment: Int, offset: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // ignore alignment for now
      val mem = thread.memory(0)
      val c = thread.popDouble()
      val i = thread.popInt()
      val ea = i + offset
      if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
        throw new TrapException(thread, "out of bounds memory access")
      } else {
        mem.unsafeWriteDouble(ea, c)
        Continue
      }
    }
  }

  case object MemorySize extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val mem = thread.memory(0)
      val sz = mem.size / pageSize
      thread.pushInt(sz)
      Continue
    }
  }

  case object MemoryGrow extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val mem = thread.memory(0)
      val sz = mem.size / pageSize
      val n = thread.popInt()
      if (mem.unsafeGrow(n))
        thread.pushInt(sz)
      else
        thread.pushInt(-1)
      Continue
    }
  }

  case object Nop extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      Continue
    }
  }

  case object Unreachable extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] =
      throw new TrapException(thread, "unreachable executed")
  }

  class Jump(var addr: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      thread.jumpTo(addr)
      Continue
    }
  }

  class JumpIf(var addr: Int) extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // read the condition from the stack
      val c = thread.popBool()
      if (c) {
        // only jump if condition is true
        thread.jumpTo(addr)
      }
      Continue
    }
  }

  sealed abstract class Breaking extends AsmInst[F] {

    protected def br(thread: ThreadFrame[F], arity: Int, drop: Int, addr: Int): Continuation[F] = {
      val res = thread.popValues(arity)
      thread.drop(drop)
      thread.pushValues(res)
      thread.jumpTo(addr)
      Continue
    }

  }

  class Br(arity: Int, drop: Int, var addr: Int) extends Breaking {
    def execute(thread: ThreadFrame[F]): Continuation[F] =
      br(thread, arity, drop, addr)
  }

  class BrIf(arity: Int, drop: Int, var addr: Int) extends Breaking {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val c = thread.popBool()
      if (c) {
        // only break if condition is true
        br(thread, arity, drop, addr)
      } else {
        // otherwise increment program counter and
        Continue
      }
    }
  }

  class BrLabel(val arity: Int, val drop: Int, var addr: Int)

  class BrTable(lbls: Array[BrLabel], dflt: BrLabel) extends Breaking {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      // get the label index from stack
      val i = thread.popInt()
      // fix the index to default to the default label
      val lbl =
        if (i >= 0 && i < lbls.length)
          lbls(i)
        else
          dflt
      br(thread, lbl.arity, lbl.drop, lbl.addr)
    }
  }

  case object Return extends AsmInst[F] {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val values = thread.popValues(thread.arity)
      // pop the thread to get the parent
      thread.popFrame()
      Done(values.headOption)
    }
  }

  sealed abstract class Invoking extends AsmInst[F] {
    protected def invoke(thread: ThreadFrame[F], f: Function[F]): Continuation[F] =
      f match {
        case inst @ FunctionInstance(_, _, _, _) =>
          // parameters are on top of the stack
          thread.pushFrame(inst)
          Continue
        case _ =>
          // pop the parameters from the stack
          val rawparams = thread.popValues(f.tpe.params.size)
          // convert parameters according to the type defined for function parameters
          val params = f.tpe.params.zip(rawparams).map {
            case (tpe, v) => Value.fromRaw(tpe, v)
          }
          // invoke the host function with the parameters
          Suspend(f.invoke(params.toVector, thread.memoryOpt(0)).map(_.map(Value.toRaw(_))))
      }
  }

  class Call(fidx: Int) extends Invoking {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val f = thread.func(fidx)
      invoke(thread, f)
    }
  }

  class CallIndirect(tidx: Int) extends Invoking {
    def execute(thread: ThreadFrame[F]): Continuation[F] = {
      val tab = thread.table(0)
      val expectedt = thread.module.types(tidx)
      val i = thread.popInt()
      if (i < 0 || i >= tab.size) {
        throw new TrapException(thread, "undefined element")
      } else if (tab(i) == null) {
        throw new TrapException(thread, s"uninitialized element $i")
      } else {
        val f = tab(i)
        val actualt = f.tpe
        if (expectedt != actualt) {
          throw new TrapException(thread, "indirect call type mismatch")
        } else {
          invoke(thread, f)
        }
      }
    }
  }

}
