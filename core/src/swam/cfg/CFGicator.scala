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
import traversal._

import cats._
import cats.implicits._

private class CFGicator[F[_]](val builder: CFGBuilder)(implicit F: MonadError[F, Throwable]) {

  private case class Context(parent: Option[Context],
                             currentBasicBlock: builder.BasicBlockBuilder,
                             continuation: builder.BasicBlockBuilder,
                             elseBB: Option[builder.BasicBlockBuilder],
                             reachable: Boolean) {
    def continuation(idx: Int): Option[builder.BasicBlockBuilder] =
      if (idx == 0)
        Some(continuation)
      else
        parent.flatMap(_.continuation(idx - 1))
  }

  private object traverser extends Traverser[F, Context] {

    override val blockPrepare: (Context, Block) => F[Context] = {
      case (ctx, _) if ctx.reachable =>
        // jump to new basic block, representing the start of this new block
        val newBB = ctx.currentBasicBlock.jumpToNew("block")
        // add a new continuation basic block
        val cont = builder.addBasicBlock("next_block")
        F.pure(Context(Some(ctx), newBB, cont, None, true))
      case (ctx, _) =>
        F.pure(ctx)
    }

    override val blockTraverse: (Context, Block) => F[Context] = {
      case (ctx, _) =>
        // add a jump to the continuation
        ctx.currentBasicBlock.jumpTo(ctx.continuation)
        val cont = ctx.continuation
        ctx.parent match {
          case Some(ctx) =>
            F.pure(ctx.copy(currentBasicBlock = cont))
          case None =>
            // this should never happen
            F.raiseError(new Exception("malformed graph"))
        }
    }

    override val loopPrepare: (Context, Loop) => F[Context] = {
      case (ctx, _) if ctx.reachable =>
        // jump to new basic block, representing the start of this new block
        val newBB = ctx.currentBasicBlock.jumpToNew("loop")
        // the loop start is also the loop continuation
        F.pure(Context(Some(ctx), newBB, newBB, None, true))
      case (ctx, _) =>
        F.pure(ctx)
    }

    override val loopTraverse: (Context, Loop) => F[Context] = {
      case (ctx, _) =>
        // add a jump to the block after the loop
        val newBB = ctx.currentBasicBlock.jumpToNew("next_loop")
        ctx.parent match {
          case Some(ctx) =>
            F.pure(ctx.copy(currentBasicBlock = newBB))
          case None =>
            // this should never happen
            F.raiseError(new Exception("malformed graph"))
        }
    }

    override val thenPrepare: (Context, If) => F[Context] = {
      case (ctx, _) if ctx.reachable =>
        // jump to new basic block, representing the start of the then branch
        val (thenBB, elseBB) = ctx.currentBasicBlock.conditionallyJumpToNew()
        // add a new continuation basic block
        val cont = builder.addBasicBlock("next_if")
        F.pure(Context(Some(ctx), thenBB, cont, Some(elseBB), true))
      case (ctx, _) =>
        F.pure(ctx)
    }

    override val elsePrepare: (Context, If) => F[Context] = {
      case (ctx, _) =>
        // add a jump to the continuation at the end of the then branch (current BB)
        ctx.currentBasicBlock.jumpTo(ctx.continuation)
        // the continuation of the else branch is the same as the one from then branch, save it
        val cont = ctx.continuation
        (ctx.parent, ctx.elseBB) match {
          case (Some(ctx), Some(elseBB)) =>
            // continue with the already linked else BB
            F.pure(Context(Some(ctx), elseBB, cont, None, true))
          case _ =>
            // this should never happen
            F.raiseError(new Exception("malformed graph"))
        }
    }

    override val ifTraverse: (Context, If) => F[Context] = {
      case (ctx, _) =>
        // add a jump to the continuation
        ctx.currentBasicBlock.jumpTo(ctx.continuation)
        val cont = ctx.continuation
        ctx.parent match {
          case Some(ctx) =>
            F.pure(ctx.copy(currentBasicBlock = cont))
          case None =>
            // this should never happen
            F.raiseError(new Exception("malformed graph"))
        }
    }

    override val otherPrepare: (Context, Inst) => F[Context] = {
      case (ctx, Br(lblIdx)) if ctx.reachable =>
        ctx.continuation(lblIdx) match {
          case Some(bb) =>
            ctx.currentBasicBlock.jumpTo(bb)
            F.pure(ctx.copy(reachable = false))
          case None =>
            F.raiseError(new Exception(s"invalid label index $lblIdx"))
        }
      case (ctx, BrIf(lblIdx)) if ctx.reachable =>
        ctx.continuation(lblIdx) match {
          case Some(bb) =>
            val newBB = ctx.currentBasicBlock.conditionallyJumpBackOrElseNew(bb)
            F.pure(ctx.copy(currentBasicBlock = newBB))
          case None =>
            F.raiseError(new Exception(s"invalid label index $lblIdx"))
        }
      case (ctx, BrTable(table, dflt)) =>
        val bbs = table.map(ctx.continuation(_))
        val dfltBb = ctx.continuation(dflt)
        val errors = (table :+ dflt).zip(bbs :+ dfltBb).collect { case (idx, None) => idx }
        if (errors.isEmpty) {
          ctx.currentBasicBlock.addJumpTable(bbs.flatten, dfltBb.get)
          F.pure(ctx.copy(reachable = false))
        } else {
          F.raiseError(new Exception(s"invalid label index(es): ${errors.mkString(", ")}"))
        }
      case (ctx, inst @ (Return | Unreachable)) if ctx.reachable =>
        ctx.currentBasicBlock.addReturn()
        F.pure(ctx.copy(reachable = false))
      case (ctx, inst) =>
        if (ctx.reachable)
          ctx.currentBasicBlock.addInst(inst)
        F.pure(ctx)
    }

  }
}

object CFGicator {

  def buildCFG[F[_]](insts: Expr)(implicit F: MonadError[F, Throwable]): F[CFG] = {
    val cfgicator = new CFGicator[F](CFGBuilder.make())
    val ctx = new cfgicator.Context(None, cfgicator.builder.entry, cfgicator.builder.end, None, true)
    insts.foldLeftM(ctx)(cfgicator.traverser.run).map { ctx =>
      ctx.currentBasicBlock.addReturn()
      cfgicator.builder.result()
    }
  }

}
