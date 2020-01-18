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
import scala.annotation.tailrec

class CFGBuilder private {

  class BasicBlockBuilder private[CFGBuilder] (val name: String) {

    private[CFGBuilder] val lbl: Int = states.length

    states += this

    private[CFGBuilder] val stmts = ListBuffer.empty[Inst]

    private[CFGBuilder] var jump = Option.empty[Jump]

    private val predecessors = ListBuffer.empty[Int]

    def addInst(i: Inst): Unit =
      stmts += i

    def jumpTo(tgt: BasicBlockBuilder): Unit = if (jump.isEmpty) {
      jump = Some(Jump.To(tgt.lbl))
      tgt.predecessors.addOne(lbl)
    }

    def jumpToNew(name: String): BasicBlockBuilder = {
      val tgt = new BasicBlockBuilder(name)
      jumpTo(tgt)
      tgt
    }

    def conditionallyJumpTo(thenTgt: BasicBlockBuilder, elseTgt: BasicBlockBuilder): Unit = if (jump.isEmpty) {
      jump = Some(Jump.If(thenTgt.lbl, elseTgt.lbl))
      thenTgt.predecessors.addOne(lbl)
      elseTgt.predecessors.addOne(lbl)
    }

    def conditionallyJumpToNew(): (BasicBlockBuilder, BasicBlockBuilder) = {
      val thenTgt = new BasicBlockBuilder("then")
      val elseTgt = new BasicBlockBuilder("else")
      conditionallyJumpTo(thenTgt, elseTgt)
      (thenTgt, elseTgt)
    }

    def conditionallyJumpBackOrElseNew(thenTgt: BasicBlockBuilder): BasicBlockBuilder = {
      val elseTgt = new BasicBlockBuilder("br_else")
      conditionallyJumpTo(thenTgt, elseTgt)
      elseTgt
    }

    def addJumpTable(table: Vector[BasicBlockBuilder], default: BasicBlockBuilder): Unit = if (jump.isEmpty) {
      jump = Some(Jump.Table(table.map(_.lbl), default.lbl))
      table.foreach(_.predecessors.addOne(lbl))
      default.predecessors.addOne(lbl)
    }

    def addReturn(): Unit =
      jumpTo(end)

    private[CFGBuilder] def result(): BasicBlock =
      BasicBlock(lbl, name, stmts.result(), jump)(predecessors.result().distinct)
  }

  private val states = ArrayBuilder.make[BasicBlockBuilder]

  /** The single entry basic block. */
  val entry: BasicBlockBuilder = new BasicBlockBuilder("entry")

  val end: BasicBlockBuilder = new BasicBlockBuilder("exit")

  def addBasicBlock(name: String): BasicBlockBuilder =
    new BasicBlockBuilder(name)

  private def postorder[Res](basicBlocks: Array[BasicBlock])(zero: Res)(f: (Res, BasicBlock) => Res): Res = {
    // this works in two steps:
    //  1. the nodes are first discovered, stacking up nodes in order of traversal
    //  2. the nodes are then aggregated when it has no undiscovered successors anymore
    //     and is aggregated if this is the first time
    @tailrec
    def loop(stack: List[Int], discovered: Set[Int], aggregated: Set[Int], acc: Res): Res =
      stack match {
        case node :: rest =>
          // add successors to the stack
          val succ = basicBlocks(node).successors.filterNot(discovered.contains(_))
          if (succ.isEmpty)
            // no successors, apply f if this is the first time we aggregate this node and continue
            loop(rest,
                 discovered + node,
                 aggregated + node,
                 if (aggregated.contains(node)) acc else f(acc, basicBlocks(node)))
          else
            // we have successors, stack them and continue
            loop(succ ++ stack, discovered + node, aggregated, acc)
        case Nil =>
          acc
      }
    loop(List(0), Set.empty, Set.empty, zero)
  }
  private def withPostorderIds(basicBlocks: Array[BasicBlock]): CFG = {
    val (id2postorder, _) = postorder(basicBlocks)((Map.empty[Int, Int], 0)) {
      case ((acc, idx), block) => (acc.updated(block.id, idx), idx + 1)
    }
    def rejump(jump: Jump): Jump =
      jump match {
        case Jump.To(lbl)            => Jump.To(id2postorder(lbl))
        case Jump.Table(cases, dflt) => Jump.Table(cases.map(id2postorder), id2postorder(dflt))
        case Jump.If(t, f)           => Jump.If(id2postorder(t), id2postorder(f))
      }
    val finalArray = Array.ofDim[BasicBlock](basicBlocks.length)
    postorder(basicBlocks)(0) {
      case (idx, bb @ BasicBlock(id, name, stmts, jump)) =>
        finalArray(idx) = BasicBlock(id2postorder(id), name, stmts, jump.map(rejump))(bb.predecessors.map(id2postorder))
        idx + 1
    }
    new CFG(finalArray)
  }

  def result(): CFG = {
    val states1 = states.result()
    // add return jump to each non-exit state without jump
    for {
      state <- states1
      if state ne end
      if state.jump.isEmpty
    } state.addReturn()
    withPostorderIds(states1.map(_.result()))
  }

}

object CFGBuilder {

  def make(): CFGBuilder =
    new CFGBuilder

}
