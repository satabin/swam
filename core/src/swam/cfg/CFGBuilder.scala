/*
 * Copyright 2019 Lucas Satabin
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
package cfg

import syntax._

import scala.collection.mutable.{ArrayBuilder, ListBuffer}

class CFGBuilder private {

  class BasicBlockBuilder private[CFGBuilder] (name: String) {

    private val lbl: Int = states.length

    states += this

    private[CFGBuilder] val stmts = ListBuffer.empty[Inst]

    private[CFGBuilder] var jump = Option.empty[Jump]

    def addInst(i: Inst): Unit =
      stmts += i

    def jumpTo(tgt: BasicBlockBuilder): Unit =
      jump = Some(Jump.To(tgt.lbl))

    def jumpToNew(name: String): BasicBlockBuilder = {
      val tgt = new BasicBlockBuilder(name)
      jumpTo(tgt)
      tgt
    }

    def conditionallyJumpTo(thenTgt: BasicBlockBuilder, elseTgt: BasicBlockBuilder): Unit =
      jump = Some(Jump.If(thenTgt.lbl, elseTgt.lbl))

    def conditionallyJumpToNew(): (BasicBlockBuilder, BasicBlockBuilder) = {
      val thenTgt = new BasicBlockBuilder("then")
      val elseTgt = new BasicBlockBuilder("else")
      conditionallyJumpTo(thenTgt, elseTgt)
      (thenTgt, elseTgt)
    }

    def conditionallyJumpBackOrElseNew(thenTgt: BasicBlockBuilder): BasicBlockBuilder = {
      val elseTgt = new BasicBlockBuilder("else")
      conditionallyJumpTo(thenTgt, elseTgt)
      elseTgt
    }

    def addJumpTable(table: Vector[BasicBlockBuilder], default: BasicBlockBuilder): Unit =
      jump = Some(Jump.Table(table.map(_.lbl), default.lbl))

    def addReturn(): Unit =
      jumpTo(end)

    private[CFGBuilder] def result(): BasicBlock =
      BasicBlock(name, stmts.result(), jump)
  }

  private val states = ArrayBuilder.make[BasicBlockBuilder]

  /** The single entry basic block. */
  val entry: BasicBlockBuilder = new BasicBlockBuilder("entry")

  val end: BasicBlockBuilder = new BasicBlockBuilder("exit")

  def addBasicBlock(name: String): BasicBlockBuilder =
    new BasicBlockBuilder(name)

  def result(): CFG = {
    val states1 = states.result()
    // add return jump to each non-exit state without jump
    for {
      state <- states1
      if state ne end
      if state.jump.isEmpty
    } state.addReturn()
    new CFG(states1.map(_.result()))
  }

}

object CFGBuilder {

  def make(): CFGBuilder =
    new CFGBuilder

}
