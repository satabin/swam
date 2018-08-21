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

import config._

import cats._

import scala.annotation.{tailrec, switch}

import java.nio.ByteBuffer

import java.lang.{Float => JFloat, Double => JDouble}

import scala.language.higherKinds

/** A call frame containing the local stack of operands.
  */
sealed class Frame[F[_]] private (parent: Frame[F],
                                  stackSize: Int,
                                  callDepth: Int,
                                  depth: Int,
                                  code: ByteBuffer,
                                  private[interpreter] val locals: Array[Long],
                                  private[interpreter] val arity: Int,
                                  private[interpreter] val instance: Instance[F]) {
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
      ttop += 1
    }

    @inline
    private def popType(): Byte = {
      ttop -= 1
      tstack(ttop)
    }

    @inline
    private def peekType(): Byte =
      tstack(ttop - 1)

    def pushBool(b: Boolean): Unit =
      pushInt(if (b) 1 else 0)

    def pushInt(i: Int): Unit =
      pushValue(i & 0x00000000ffffffffl)

    def pushLong(l: Long): Unit =
      pushValue(l)

    def pushFloat(f: Float): Unit =
      pushValue(JFloat.floatToRawIntBits(f) & 0x00000000ffffffffl)

    def pushDouble(d: Double): Unit =
      pushValue(JDouble.doubleToRawLongBits(d))

    def popBool(): Boolean =
      popInt() != 0

    def popInt(): Int =
      (popValue() & 0x00000000ffffffffl).toInt

    def peekInt(): Int =
      (peekValue() & 0x00000000ffffffffl).toInt

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

    def drop(): Unit =
      popLong()

    def popValue(): Long = {
      val tpe = popType()
      if(tpe == LABEL)
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
      if(tpe != LABEL)
        throw new Exception("Malformed stack")
      vtop -= 1
      ltop -= 1
      lstack(ltop)
    }

    def getLabel(idx: Int): Label =
      lstack(ltop - 1 - idx)

    def pushFrame(arity: Int, code: ByteBuffer, locals: Array[Long], instance: Instance[F])(
        implicit F: MonadError[F, Throwable]): F[Frame[F]] =
      if (depth < callDepth)
        F.pure(new Frame[F](self, stackSize, callDepth, depth + 1, code, locals, arity, instance))
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

  def makeToplevel[F[_]](instance: Instance[F], conf: EngineConfiguration): Frame[F] =
    new Frame[F](null, conf.stack.height, conf.stack.callDepth, 0, null, null, 0, instance)

}
