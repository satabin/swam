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

import scala.annotation.{tailrec, switch}

import java.nio.ByteBuffer

import scala.language.higherKinds

/** A call frame containing the local stack of operands.
  */
sealed class Frame[F[_]] private (parent: Frame[F],
                                  stackSize: Int,
                                  code: ByteBuffer,
                                  val locals: Array[Value],
                                  val arity: Int,
                                  val instance: Instance[F]) {
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

    private val stack = Array.ofDim[Byte](stackSize)
    private val istack = Array.ofDim[Int](stackSize)
    private val lstack = Array.ofDim[Long](stackSize)
    private val fstack = Array.ofDim[Float](stackSize)
    private val dstack = Array.ofDim[Double](stackSize)

    private val lblstack = Array.ofDim[Long](stackSize)

    private var top = 0
    private var itop = 0
    private var ltop = 0
    private var ftop = 0
    private var dtop = 0

    private var lbltop = 0

    @inline
    private def push(tpe: Byte): Unit = {
      stack(top) = tpe
      top += 1
    }

    @inline
    private def pop(): Byte = {
      top -= 1
      stack(top)
    }

    @inline
    private def check(actual: Byte, expected: Byte): Unit =
      if (actual != expected)
        throw new SwamException("Malformed stack")

    def pushBool(b: Boolean): Unit =
      pushInt(if (b) 1 else 0)

    def pushInt(i: Int): Unit = {
      push(INT32)
      istack(itop) = i
      itop += 1
    }

    def pushLong(l: Long): Unit = {
      push(INT64)
      lstack(ltop) = l
      ltop += 1
    }

    def pushFloat(f: Float): Unit = {
      push(FLOAT32)
      fstack(ftop) = f
      ftop += 1
    }

    def pushDouble(d: Double): Unit = {
      push(FLOAT64)
      dstack(dtop) = d
      dtop += 1
    }

    def popBool(): Boolean =
      popInt() != 0

    def popInt(): Int = {
      check(pop(), INT32)
      itop -= 1
      istack(itop)
    }

    def popLong(): Long = {
      check(pop(), INT64)
      ltop -= 1
      lstack(ltop)
    }

    def popFloat(): Float = {
      check(pop(), FLOAT32)
      ftop -= 1
      fstack(ftop)
    }

    def popDouble(): Double = {
      check(pop(), FLOAT64)
      dtop -= 1
      dstack(dtop)
    }

    def drop(): Unit =
      (stack(top - 1): @switch) match {
        case INT32   => popInt()
        case INT64   => popLong()
        case FLOAT32 => popFloat()
        case FLOAT64 => popDouble()
        case _       => throw new SwamException("Malformed stack")
      }

    def popValue(): Value =
      (stack(top - 1): @switch) match {
        case INT32   => Value.Int32(popInt())
        case INT64   => Value.Int64(popLong())
        case FLOAT32 => Value.Float32(popFloat())
        case FLOAT64 => Value.Float64(popDouble())
        case _       => throw new SwamException("Malformed stack")
      }

    def popValues(): Seq[Value] = {
      @tailrec
      def loop(acc: Seq[Value]): Seq[Value] =
        if (top == 0 || stack(top - 1) > 3)
          acc
        else
          loop(popValue() +: acc)
      loop(Seq.empty)
    }

    def popValues(n: Int): Seq[Value] = {
      @tailrec
      def loop(n: Int, acc: Seq[Value]): Seq[Value] =
        if (n <= 0)
          acc
        else
          loop(n - 1, popValue() +: acc)
      loop(n, Seq.empty)
    }

    def pushValue(v: Value): Unit =
      v match {
        case Value.Int32(i)   => pushInt(i)
        case Value.Int64(l)   => pushLong(l)
        case Value.Float32(f) => pushFloat(f)
        case Value.Float64(d) => pushDouble(d)
      }

    def pushValues(values: Seq[Value]): Unit =
      values.foreach(pushValue(_))

    def pushLabel(lbl: Label): Unit = {
      push(LABEL)
      lblstack(lbltop) = lbl
      lbltop += 1
    }

    def popLabel(): Label = {
      check(pop(), LABEL)
      lbltop -= 1
      lblstack(lbltop)
    }

    def getLabel(idx: Int): Label =
      lblstack(lbltop - 1 - idx)

    def pushFrame(arity: Int, code: ByteBuffer, locals: Array[Value], instance: Instance[F]): Frame[F] =
      new Frame[F](self, stackSize, code, locals, arity, instance)

    def popFrame(): Frame[F] =
      parent
  }

  private[interpreter] def isToplevel: Boolean =
    parent == null

}

object Frame {

  private final val INT32 = 0
  private final val INT64 = 1
  private final val FLOAT32 = 2
  private final val FLOAT64 = 3
  private final val LABEL = 4

  def makeToplevel[F[_]](instance: Instance[F], conf: EngineConfiguration): Frame[F] =
    new Frame[F](null, conf.stack.height, null, null, 0, instance)

}
