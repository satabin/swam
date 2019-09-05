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
package high

import config._

import cats._

import scala.annotation.tailrec

import java.nio.ByteBuffer

import java.lang.{Float => JFloat, Double => JDouble}

import scala.language.higherKinds

/** A call frame containing the local stack of operands.
  */
sealed class Frame[F[_]] private (
    parent: Frame[F],
    stackSize: Int,
    callDepth: Int,
    depth: Int,
    code: ByteBuffer,
    private[interpreter] val locals: Array[Long],
    private[interpreter] val arity: Int,
    private[interpreter] val instance: Instance[F],
    tracer: Tracer)(implicit F: MonadError[F, Throwable])
    extends StackFrame {
  self =>

  import Frame._

  private[interpreter] var pc = 0

  private[interpreter] def readByte(): Byte = {
    val b = code.get(pc)
    pc += 1
    b
  }

  private[interpreter] def readInt(): Int = {
    val i = code.getInt(pc)
    pc += 4
    i
  }

  private[interpreter] def readLong(): Long = {
    val l = code.getLong(pc)
    pc += 8
    l
  }

  private[interpreter] def readFloat(): Float = {
    val f = code.getFloat(pc)
    pc += 4
    f
  }

  private[interpreter] def readDouble(): Double = {
    val d = code.getDouble(pc)
    pc += 8
    d
  }

  private[interpreter] object stack {

    private val tstack = Array.ofDim[Byte](stackSize)
    private val vstack = Array.ofDim[Long](stackSize)
    private val lstack = Array.ofDim[Long](stackSize)

    private var ttop = 0
    private var vtop = 0
    private var ltop = 0

    def clear(): Unit = {
      ttop = 0
      vtop = 0
      ltop = 0
    }

    @inline
    private def pushType(tpe: Byte): Unit = {
      tstack(ttop) = tpe

      if (tracer != null) tracer.traceEvent("spush", "Type", tpe)

      ttop += 1
    }

    @inline
    private def popType(): Byte = {
      ttop -= 1
      val r = tstack(ttop)
      if (tracer != null) tracer.traceEvent("spop", "Type", r)
      r
    }

    @inline
    private def peekType(): Byte =
      tstack(ttop - 1)

    def pushBool(b: Boolean): Unit = {
      if (tracer != null) tracer.traceEvent("spush", "i32", if (b) 1 else 0)
      pushInt(if (b) 1 else 0)
    }

    def pushInt(i: Int): Unit = {
      if (tracer != null) tracer.traceEvent("spush", "i32", i & 0x00000000ffffffffl)
      pushValue(i & 0x00000000ffffffffl)
    }

    def pushLong(l: Long): Unit = {
      if (tracer != null) tracer.traceEvent("spush", "i64", l)
      pushValue(l)
    }

    def pushFloat(f: Float): Unit = {
      if (tracer != null) tracer.traceEvent("spush", "f32", JFloat.floatToRawIntBits(f) & 0x00000000ffffffffl)
      pushValue(JFloat.floatToRawIntBits(f) & 0x00000000ffffffffl)
    }

    def pushDouble(d: Double): Unit = {
      if (tracer != null) tracer.traceEvent("spush", "f64", JDouble.doubleToRawLongBits(d))
      pushValue(JDouble.doubleToRawLongBits(d))
    }

    def popBool(): Boolean = {
      val r = popInt()
      if (tracer != null) tracer.traceEvent("spop", "i32", r)
      r != 0
    }

    def popInt(): Int = {
      val r = (popValue() & 0x00000000ffffffffl).toInt
      if (tracer != null) tracer.traceEvent("spop", "i32", r)
      r
    }

    def peekInt(): Int =
      (peekValue() & 0x00000000ffffffffl).toInt

    def popLong(): Long = {
      val r = popValue()
      if (tracer != null) tracer.traceEvent("spop", "i64", r)
      r
    }

    def peekLong(): Long =
      peekValue()

    def popFloat(): Float = {
      val r = JFloat.intBitsToFloat(popInt())
      if (tracer != null) tracer.traceEvent("spop", "f32", r)
      r
    }

    def peekFloat(): Float =
      JFloat.intBitsToFloat(peekInt())

    def popDouble(): Double = {
      val r = JDouble.longBitsToDouble(popLong())
      if (tracer != null) tracer.traceEvent("spop", "f64", r)
      r
    }

    def peekDouble(): Double =
      JDouble.longBitsToDouble(peekLong())

    def drop(): Unit = {
      val r = popLong()
      if (tracer != null) tracer.traceEvent("spop", "i64", "drop", r)
    }

    def popValue(): Long = {
      val tpe = popType()
      if (tpe == LABEL)
        throw new Exception("Malformed stack")
      vtop -= 1
      vstack(vtop)

    }

    def peekValue(): Long =
      vstack(vtop - 1)

    def popValues(): Seq[Long] = {
      @tailrec
      def loop(acc: Seq[Long]): Seq[Long] =
        if (ttop <= 0 || tstack(ttop - 1) == LABEL)
          acc
        else
          loop(popValue() +: acc)
      loop(Seq.empty)
    }

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
      pushType(VALUE)
      vstack(vtop) = l
      vtop += 1
    }

    def pushValues(values: Seq[Long]): Unit =
      values.foreach(pushValue(_))

    def pushLabel(lbl: Label): Unit = {
      pushType(LABEL)
      vtop += 1
      lstack(ltop) = lbl
      ltop += 1
    }

    def popLabel(): Label = {
      val tpe = popType()
      if (tpe != LABEL)
        throw new Exception("Malformed stack")
      vtop -= 1
      ltop -= 1
      lstack(ltop)
    }

    def getLabel(idx: Int): Label =
      lstack(ltop - 1 - idx)

    def pushFrame(arity: Int, code: ByteBuffer, locals: Array[Long], instance: Instance[F]): F[Frame[F]] =
      if (depth < callDepth)
        F.pure(new Frame[F](self, stackSize, callDepth, depth + 1, code, locals, arity, instance, tracer))
      else
        F.raiseError(new StackOverflowException(self))

    def popFrame(): Frame[F] =
      parent
  }

  private[interpreter] def isToplevel: Boolean =
    parent == null

}

object Frame {

  private final val VALUE = 0
  private final val LABEL = 1

  def makeToplevel[F[_]](instance: Instance[F], conf: HighLevelStackConfiguration, tracer: Tracer)(implicit F: MonadError[F, Throwable]): Frame[F] =
    new Frame[F](null, conf.size.toBytes.toInt, conf.callDepth, 0, null, null, 0, instance, tracer)

}
