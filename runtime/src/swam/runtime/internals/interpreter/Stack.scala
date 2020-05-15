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

import instance._
import config._

import scala.annotation.tailrec

import java.lang.{Float => JFloat, Double => JDouble}

/** Low-level structures that represents the stack and registers of
  * a thread in the interpreter.
  * Each thread gets its own stack.
  * When function calls are made, a new frame is pushed on the stack
  * with saved registers to allow for returns.
  *
  * Following registers are maintained:
  *  - `pc` the program counter that points to the next instruction to execute
  *  - `fp` the frame poiter that points to the current frame base on the stack
  *  - `tp` the tp of the stack
  *
  * The stack structure is as follows:
  * {{{
  * .           .
  * .           .
  * |           |
  * +-----------+
  * |  local 0  |
  * +-----------+
  * |  local 1  |
  * +-----------+
  * |    ...    |
  * .           .
  * .           .
  * +-----------+
  * |  local n  |
  * +-----------+
  * | nb locals | <- fp
  * +-----------+
  * |   arity   |
  * +-----------+
  * | return pc |
  * +-----------+
  * | return fp |
  * +-----------+
  * |    ...    |
  * .           .
  * .           .
  * +-----------+
  * |           | <- tp
  * +-----------+
  * |           |
  * .           .
  * .           .
  * }}}
  *
  */
private[runtime] class ThreadFrame[F[_]](conf: StackConfiguration, baseInstance: Instance[F]) extends Frame[F] {

  private val stack = Array.ofDim[Long](conf.size.bytes.toInt / 8)

  private var fp = 0

  private var tp = 0

  private var pc = 0

  private var code: Array[AsmInst[F]] = _

  private var instances: List[FunctionInstance[F]] = Nil

  def instance: FunctionInstance[F] = instances.head

  def clearStack(): Unit = {
    fp = 0
    tp = 0
  }

  def arity: Int =
    (stack(fp + 1) & 0xffffffff).toInt

  def nbLocals: Int =
    (stack(fp) & 0xffffffff).toInt

  def isToplevel: Boolean =
    instances.isEmpty

  def pushFrame(fun: FunctionInstance[F]): Unit = {
    val nbLocals = fun.locals.size
    // push the current function instance on the instance stack
    instances = fun :: instances
    // set the current code to execute to the new function body
    code = fun.code
    // make room for locals
    tp += nbLocals
    pushInt(nbLocals + fun.tpe.params.size)
    pushInt(fun.tpe.t.size)
    pushInt(pc)
    pushInt(fp)
    pc = 0
    fp = tp - 4
  }

  def popFrame(): Unit = {
    // pop the function instance
    instances = instances.tail
    //previous.next = null
    // restore the registers
    tp = fp - nbLocals
    pc = (stack(fp + 2) & 0xffffffff).toInt
    fp = (stack(fp + 3) & 0xffffffff).toInt
    // restore the code
    if (instances.isEmpty) {
      code = null
    } else {
      code = instances.head.code
    }
  }

  def pushBool(b: Boolean): Unit =
    pushInt(if (b) 1 else 0)

  def pushInt(i: Int): Unit =
    pushValue(i & 0X00000000FFFFFFFFL)

  def pushLong(l: Long): Unit =
    pushValue(l)

  def pushFloat(f: Float): Unit =
    pushValue(JFloat.floatToRawIntBits(f) & 0X00000000FFFFFFFFL)

  def pushDouble(d: Double): Unit =
    pushValue(JDouble.doubleToRawLongBits(d))

  def popBool(): Boolean =
    popInt() != 0

  def popInt(): Int =
    (popValue() & 0X00000000FFFFFFFFL).toInt

  def peekInt(): Int =
    (peekValue() & 0X00000000FFFFFFFFL).toInt

  def popLong(): Long =
    popValue()

  def peekLong(): Long =
    peekValue()

  def popFloat(): Float =
    JFloat.intBitsToFloat(popInt())

  def peekFloat(): Float =
    JFloat.intBitsToFloat(peekInt())

  def popDouble(): Double =
    JDouble.longBitsToDouble(popLong())

  def peekDouble(): Double =
    JDouble.longBitsToDouble(peekLong())

  def drop(n: Int): Unit =
    tp -= n

  def popValue(): Long = {
    tp -= 1
    stack(tp)
  }

  def peekValue(): Long =
    stack(tp - 1)

  def popValues(n: Int): Seq[Long] = {
    @tailrec
    def loop(n: Int, acc: Seq[Long]): Seq[Long] =
      if (n <= 0)
        acc
      else
        loop(n - 1, popValue() +: acc)
    loop(n, Seq.empty)
  }

  def pushValue(l: Long): Unit = {
    stack(tp) = l
    tp += 1
  }

  def pushValues(values: Seq[Long]): Unit =
    values.foreach(pushValue(_))

  def fetch(): AsmInst[F] = {
    val inst = code(pc)
    pc += 1
    inst
  }

  def jumpTo(idx: Int): Unit =
    pc = idx

  def local(idx: Int): Long =
    stack(fp - idx)

  def setLocal(idx: Int, v: Long): Unit =
    stack(fp - idx) = v

  def global(idx: Int): Global[F] =
    instance.instance.globals(idx)

  def memory(idx: Int): Memory[F] =
    instance.instance.memories(idx)

  def func(fidx: Int): Function[F] =
    instance.instance.funcs(fidx)

  def table(idx: Int): Table[F] =
    instance.instance.tables(idx)

  def module: Module[F] =
    instance.instance.module

  def memoryOpt(idx: Int): Option[Memory[F]] =
    if (isToplevel)
      baseInstance.memories.lift(idx)
    else
      instance.instance.memories.lift(idx)

  def functionName: Option[String] =
    instance.name

  def moduleName: Option[String] =
    instance.instance.module.name

}
