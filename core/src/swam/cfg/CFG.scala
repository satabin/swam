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

import cats._
import cats.implicits._

import scala.annotation.tailrec

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

  /** Returns the immediate dominators. The result is indexed by the node identifiers.
    * Each cel in the vector contains the identifier of the immediate dominator.
    */
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

  /** Returns the dominator tree for this CFG.
    * The tree contains the node identifiers.
    */
  def dominatorTree: DominatorTree =
    new DominatorTree(idoms)

}

/** The dominator tree associated to a [[CFG]] which can be traversed
  * in several ways.
  */
class DominatorTree private[cfg] (idoms: Vector[Int]) {
  private lazy val byParent = idoms.toList.zipWithIndex.groupMap {
    case (idom, node) if idom == node =>
      // the root
      None
    case (idom, _) => Some(idom)
  } {
    case (idom, node) if idom == node =>
      // the root
      (None, node)
    case (idom, node) => (Some(idom), node)
  }

  /** Traverses this tree, accumulating the result through the
    * function `f` in preorder.
    *
    * The second argument of `f` is the parent of the currently
    * processed node, if any.
    */
  def preorder[Res](zero: Res)(f: (Res, Option[Int], Int) => Res): Res = {
    @tailrec
    def loop(acc: Res, stack: List[List[(Option[Int], Int)]]): Res =
      stack match {
        case Nil :: rest =>
          loop(acc, rest)
        case ((parent, node) :: siblings) :: rest =>
          val children = byParent.getOrElse(Some(node), Nil)
          loop(f(acc, parent, node), children :: siblings :: rest)
        case Nil =>
          acc
      }
    loop(zero, List(List((None, idoms.size - 1))))
  }

  /** Monadicly traverses this tree, accumulating the result through the
    * function `f` in preorder.
    *
    * The second argument of `f` is the parent of the currently
    * processed node, if any.
    *
    * This may be used to impement short-circuit semantics.
    */
  def preorderM[F[_], Res](zero: Res)(f: (Res, Option[Int], Int) => F[Res])(implicit F: Monad[F]): F[Res] =
    F.tailRecM((zero, List(List((Option.empty[Int], idoms.size - 1))))) {
      case (acc, Nil :: rest) =>
        F.pure(Left((acc, rest)))
      case (acc, ((parent, node) :: siblings) :: rest) =>
        f(acc, parent, node).map { acc =>
          val children = byParent.getOrElse(Some(node), Nil)
          Left((acc, children :: siblings :: rest))
        }
      case (acc, Nil) =>
        F.pure(Right(acc))
    }

  /** Traverses this tree, accumulating the result through the
    * function `f` in postorder.
    *
    * The second argument of `f` is the parent of the currently
    * processed node, if any.
    */
  def postorder[Res](zero: Res)(f: (Res, Option[Int], Int) => Res): Res = {
    @tailrec
    def loop(acc: Res, stack: List[List[(Option[Int], Int)]], childrenDone: Set[Int]): Res =
      stack match {
        case Nil :: rest =>
          loop(acc, rest, childrenDone)
        case ((pair @ (parent, node)) :: siblings) :: rest =>
          if (childrenDone.contains(node)) {
            // children were processed, so do this node and continue to siblings
            loop(f(acc, parent, node), siblings :: rest, childrenDone)
          } else {
            // children were not processed yet, check if any at all
            byParent.get(Some(node)) match {
              case Some(children) =>
                // first process the children, then this node, then siblings
                // next time we will encounter this node, the children will
                // be processed and this node will be safe to process,
                // so register this in `childrenDone`
                loop(acc, children :: List(pair) :: siblings :: rest, childrenDone + node)
              case None =>
                // this is a leaf, process it, ad continue to siblings
                loop(f(acc, parent, node), siblings :: rest, childrenDone)
            }
          }
        case Nil =>
          acc
      }
    // start at the root
    loop(zero, List(List((None, idoms.size - 1))), Set.empty)
  }

  /** Monadicly traverses this tree, accumulating the result through the
    * function `f` in postorder.
    *
    * The second argument of `f` is the parent of the currently
    * processed node, if any.
    *
    * This may be used to impement short-circuit semantics.
    */
  def postorderM[F[_], Res](zero: Res)(f: (Res, Option[Int], Int) => F[Res])(implicit F: Monad[F]): F[Res] =
    F.tailRecM((zero, List(List((Option.empty[Int], idoms.size - 1))), Set.empty[Int])) {
      case (acc, Nil :: rest, childrenDone) =>
        F.pure(Left((acc, rest, childrenDone)))
      case (acc, ((pair @ (parent, node)) :: siblings) :: rest, childrenDone) =>
        if (childrenDone.contains(node)) {
          // children were processed, so do this node and continue to siblings
          f(acc, parent, node).map { acc =>
            Left((acc, siblings :: rest, childrenDone))
          }
        } else {
          // children were not processed yet, check if any at all
          byParent.get(Some(node)) match {
            case Some(children) =>
              // first process the children, then this node, then siblings
              // next time we will encounter this node, the children will
              // be processed and this node will be safe to process,
              // so register this in `childrenDone`
              F.pure(Left((acc, children :: List(pair) :: siblings :: rest, childrenDone + node)))
            case None =>
              // this is a leaf, process it, ad continue to siblings
              f(acc, parent, node).map { acc =>
                Left((acc, siblings :: rest, childrenDone))
              }
          }
        }
      case (acc, Nil, _) =>
        F.pure(Right(acc))
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
