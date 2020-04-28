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
package trace

import internals.interpreter._
import internals.instance.FunctionInstance

private[runtime] class TracingFrame[F[_]](inner: Frame[F], tracer: Tracer) extends Frame[F] {

  def instance: FunctionInstance[F] =
    inner.instance

  def clearStack(): Unit =
    inner.clearStack()

  def arity: Int =
    inner.arity

  def nbLocals: Int =
    inner.nbLocals

  def isToplevel: Boolean =
    inner.isToplevel

  def pushFrame(fun: FunctionInstance[F]): Unit =
    inner.pushFrame(fun)

  def popFrame(): Unit = {
    inner.popFrame()
  }

  def pushBool(b: Boolean): Unit = {
    tracer.trace(EventType.SPush, List("i32", if (b) "1" else "0"), this)
    inner.pushBool(b)
  }

  def pushInt(i: Int): Unit = {
    tracer.trace(EventType.SPush, List("i32", i.toString), this)
    inner.pushInt(i)
  }

  def pushLong(l: Long): Unit = {
    tracer.trace(EventType.SPush, List("i64", l.toString), this)
    inner.pushLong(l)
  }

  def pushFloat(f: Float): Unit = {
    tracer.trace(EventType.SPush, List("f32", f.toString), this)
    inner.pushFloat(f)
  }

  def pushDouble(d: Double): Unit = {
    tracer.trace(EventType.SPush, List("f64", d.toString), this)
    inner.pushDouble(d)
  }

  def popBool(): Boolean = {
    val res = inner.popBool()
    tracer.trace(EventType.SPop, List("i32", if (res) "1" else "0"), this)
    res
  }

  def popInt(): Int = {
    val res = inner.popInt()
    tracer.trace(EventType.SPop, List("i32", res.toString), this)
    res
  }

  def peekInt(): Int = {
    val res = inner.peekInt()
    tracer.trace(EventType.SPeek, List("i32", res.toString), this)
    res
  }

  def popLong(): Long = {
    val res = inner.popLong()
    tracer.trace(EventType.SPop, List("i64", res.toString), this)
    res
  }

  def peekLong(): Long = {
    val res = inner.peekLong()
    tracer.trace(EventType.SPeek, List("i64", res.toString), this)
    res
  }

  def popFloat(): Float = {
    val res = inner.popFloat()
    tracer.trace(EventType.SPop, List("f32", res.toString), this)
    res
  }

  def peekFloat(): Float = {
    val res = inner.peekFloat()
    tracer.trace(EventType.SPeek, List("f32", res.toString), this)
    res
  }

  def popDouble(): Double = {
    val res = inner.popDouble()
    tracer.trace(EventType.SPop, List("f64", res.toString), this)
    res
  }

  def peekDouble(): Double = {
    val res = inner.peekDouble()
    tracer.trace(EventType.SPeek, List("f64", res.toString), this)
    res
  }

  def drop(n: Int): Unit = {
    tracer.trace(EventType.SPop, List("i64", "drop", n.toString), this)
    inner.drop(n)
  }

  def popValue(): Long = {
    val res = inner.popValue()
    tracer.trace(EventType.SPop, List("i64", res.toString), this)
    res
  }

  def peekValue(): Long = {
    val res = inner.peekValue()
    tracer.trace(EventType.SPeek, List("i64", res.toString), this)
    res
  }

  def popValues(n: Int): Seq[Long] = {
    val res = inner.popValues(n)
    tracer.trace(EventType.SPop, "i64" :: res.map(_.toString).toList, this)
    res
  }

  def pushValue(l: Long): Unit = {
    tracer.trace(EventType.SPush, List("i64", l.toString), this)
    inner.pushValue(l)
  }

  def pushValues(values: Seq[Long]): Unit = {
    tracer.trace(EventType.SPush, "i64" :: values.map(_.toString).toList, this)
    inner.pushValues(values)
  }

  def fetch(): AsmInst[F] =
    inner.fetch()

  def jumpTo(idx: Int): Unit =
    inner.jumpTo(idx)

  def local(idx: Int): Long =
    inner.local(idx)

  def setLocal(idx: Int, v: Long): Unit =
    inner.setLocal(idx, v)

  def global(idx: Int): Global[F] =
    inner.global(idx)

  def memory(idx: Int): Memory[F] =
    inner.memory(idx)

  def func(fidx: Int): Function[F] =
    inner.func(fidx)

  def table(idx: Int): Table[F] =
    inner.table(idx)

  def module: Module[F] =
    inner.module

  def memoryOpt(idx: Int): Option[Memory[F]] =
    inner.memoryOpt(idx)

  def functionName: Option[String] =
    inner.functionName

  def moduleName: Option[String] =
    inner.moduleName

}
