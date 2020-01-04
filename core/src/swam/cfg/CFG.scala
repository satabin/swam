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

import syntax.Inst

import scala.annotation.tailrec

/** An immutable control-flow graph.
  *
  * To create a new CFG, see [[CFGBuilder]].
  */
class CFG private[cfg] (basicBlocks: Array[BasicBlock]) {

  /** Returns the basic block with given identifier.*/
  def block(block: Int): Option[BasicBlock] = basicBlocks.lift(block)

  /** The basic blocks in this control flow graph. */
  def blocks: List[BasicBlock] = basicBlocks.toList

  /** The graph entry point. */
  val entry: BasicBlock = basicBlocks(0)

  /** The graph exit point. */
  val exit: BasicBlock = basicBlocks(1)

  /** The list of successors to the given basic block. */
  def successors(block: Int): List[Int] =
    if (block < 0 || block > basicBlocks.size)
      Nil
    else
      basicBlocks(block).jump match {
        case Some(Jump.To(tgt))            => List(tgt)
        case Some(Jump.If(tTgt, eTgt))     => List(tTgt, eTgt)
        case Some(Jump.Table(cases, dflt)) => cases.toList :+ dflt
        case None                          => Nil
      }

  /** Visits the CFG basic blocks apply the aggregation function in postorder. */
  def postorder[Res](zero: Res)(f: (Res, BasicBlock) => Res): Res = {
    // this works in two steps:
    //  1. the nodes are first discovered, stacking up nodes in order of traversal
    //  2. the nodes are then aggregated when it has no undiscovered successors anymore
    //     and is aggregated if this is the first time
    @tailrec
    def loop(stack: List[Int], discovered: Set[Int], aggregated: Set[Int], acc: Res): Res =
      stack match {
        case node :: rest =>
          // add successors to the stack
          val succ = successors(node).filterNot(discovered.contains(_))
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

  /** Returns the list of basic blocks in reverse postorder. */
  def reversePostorder: List[Int] =
    postorder(List.empty[Int])((acc, node) => node.id :: acc)

}

case class BasicBlock(id: Int, name: String, stmts: List[Inst], jump: Option[Jump])
