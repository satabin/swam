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

/** An immutable control-flow graph.
  *
  * To create a new CFG, see [[CFGBuilder]].
  */
class CFG private[cfg] (basicBlocks: Array[BasicBlock]) {

  /** Returns the basic block with given identifier.*/
  def block(block: Int): Option[BasicBlock] = basicBlocks.lift(block)

  /** The basic blocks in this control flow graph in post order. */
  def blocks: List[BasicBlock] = basicBlocks.toList

  /** The graph entry point. */
  val entry: BasicBlock = basicBlocks(basicBlocks.length - 1)

  def successors(block: Int): List[Int] =
    if (block < 0 || block > basicBlocks.size)
      Nil
    else
      basicBlocks(block).successors

  def predecessors(block: Int): List[Int] =
    if (block < 0 || block > basicBlocks.size)
      Nil
    else
      basicBlocks(block).predecessors

  /** Traverses the graph in postorder and applies the function `f` to compute the accumulated result. */
  def postorder[Res](zero: Res)(f: (Res, BasicBlock) => Res): Res =
    basicBlocks.foldLeft(zero)(f)

  /** Returns the list of basic blocks in reverse postorder. */
  def reversePostorder: List[Int] =
    postorder(List.empty[Int])((acc, node) => node.id :: acc)

  /** Returns the immediate dominators. The result is indexed by the node identifiers. */
  def idoms: Vector[Int] = {
    // cache it as it will be reused for every pass
    val reversed = reversePostorder
    val doms = Array.fill[Int](basicBlocks.size)(-1)
    doms(basicBlocks.length - 1) = basicBlocks.length - 1
    def intersect(block1: Int, block2: Int): Int = {
      def inc(finger: Int, tgt: Int): Int =
        if (finger < tgt) {
          inc(doms(finger), tgt)
        } else {
          finger
        }
      def loop(finger1: Int, finger2: Int): Int =
        if (finger1 == finger2) {
          finger1
        } else {
          val finger11 = inc(finger1, finger2)
          val finger21 = inc(finger2, finger11)
          loop(finger11, finger21)
        }
      loop(block1, block2)
    }
    def findFirstProcessed(preds: List[Int]): Option[(Int, List[Int])] =
      preds match {
        case p :: ps =>
          if (doms(p) != -1)
            Some(p -> ps)
          else
            findFirstProcessed(ps)
        case Nil =>
          None
      }
    def loop(pass: Int): Unit = {
      val changed =
        reversed.tail.foldLeft(false) { (changed, p) =>
          val idom =
            findFirstProcessed(basicBlocks(p).predecessors)
              .map {
                case (p, ps) =>
                  ps.foldLeft(p) { (idom, p) =>
                    if (doms(p) != -1)
                      // already calculated
                      intersect(p, idom)
                    else
                      idom
                  }
              }
              .getOrElse(throw new Exception("this is a bug"))
          if (doms(p) != idom) {
            doms(p) = idom
            true
          } else {
            changed
          }
        }
      if (changed) loop(pass + 1)
    }
    loop(1)
    doms.toVector
  }

}

case class BasicBlock(id: Int, name: String, stmts: List[Inst], jump: Option[Jump])(val predecessors: List[Int]) {
  def successors: List[Int] =
    jump match {
      case Some(Jump.To(tgt))            => List(tgt)
      case Some(Jump.If(tTgt, eTgt))     => List(tTgt, eTgt)
      case Some(Jump.Table(cases, dflt)) => cases.toList :+ dflt
      case None                          => Nil
    }
}
