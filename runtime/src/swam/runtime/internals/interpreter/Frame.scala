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

import instance.FunctionInstance

trait Frame[F[_]] extends StackFrame {

  def instance: FunctionInstance[F]

  def clearStack(): Unit

  def arity: Int

  def nbLocals: Int

  def isToplevel: Boolean

  def pushFrame(fun: FunctionInstance[F]): Unit

  def popFrame(): Unit

  def pushBool(b: Boolean): Unit

  def pushInt(i: Int): Unit

  def pushLong(l: Long): Unit

  def pushFloat(f: Float): Unit

  def pushDouble(d: Double): Unit

  def popBool(): Boolean

  def popInt(): Int

  def peekInt(): Int

  def popLong(): Long

  def peekLong(): Long

  def popFloat(): Float

  def peekFloat(): Float

  def popDouble(): Double

  def peekDouble(): Double

  def drop(n: Int): Unit

  def popValue(): Long

  def peekValue(): Long

  def popValues(n: Int): Seq[Long]

  def pushValue(l: Long): Unit

  def pushValues(values: Seq[Long]): Unit

  def fetch(): AsmInst[F]

  def jumpTo(idx: Int): Unit

  def local(idx: Int): Long

  def setLocal(idx: Int, v: Long): Unit

  def global(idx: Int): Global[F]

  def memory(idx: Int): Memory[F]

  def func(fidx: Int): Function[F]

  def table(idx: Int): Table[F]

  def module: Module[F]

  def memoryOpt(idx: Int): Option[Memory[F]]

}
