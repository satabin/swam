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
package compiler

import syntax._
import interpreter._
import config.ConfiguredByteOrder

import cats._

import fs2._

import scala.collection.mutable.ArrayBuilder

import scala.annotation.tailrec

import java.nio.ByteBuffer
import java.nio.ByteOrder

/** A stack of labels, each one tracking following information:
  *  - the parent label
  *  - its target on break
  *  - its arity (i.e. the number of return value to pop and return on break)
  *  - the number of pushed value so far for the current label
  * At top-level, the parent is `null` and target is `0`.
  * No checks need to be performed because the compiler is only called if the
  * program is correct, in which case all breaks target an existing label.
  * The top-level label is a special synthesized one, representing the top-level
  * block of the function body.
  */
case class LabelStack(parent: LabelStack, target: Target, arity: Int, pushed: Int) {

  @inline
  def push(n: Int): LabelStack =
    copy(pushed = pushed + n)

  @inline
  def pop(n: Int): LabelStack =
    copy(pushed = pushed - n)

  @inline
  def isToplevel: Boolean =
    parent == null

  // returns the target label and the rest to pop (after the returned label arity as been popped)
  @inline
  def break(lbll: Int): (LabelStack, Int) =
    breakLoop(lbll, this, 0)

  @tailrec
  private def breakLoop(lbll: Int, elem: LabelStack, pushed: Int): (LabelStack, Int) =
    if (lbll == 0)
      // we reached the label
      (elem, pushed + elem.pushed - elem.arity)
    else
      breakLoop(lbll - 1, elem.parent, pushed + elem.pushed)

}

/** Compilation context used when compiling instructions for a function.
  * The label stack represents the static trace of the program so far in
  * terms of stack modifications, the errata registers the label to fix
  * later, and the jump label offsets.
  * This allows to compile code in one pass even with forward jumps.
  *
  * The `errata` lists how to set correct offset for target labels.
  *
  * The `offsets` maps label names to their offset.
  *
  * Once all code has been generated, the placeholders pointed by the `errata`
  * map are replaced with real offsets from the `offsets` map.
  */
case class FunctionContext(labels: LabelStack,
                           errata: List[(Target, Int => Unit)],
                           offsets: Map[Target, Int],
                           offset: Int,
                           nxt: Int) {

  @inline
  def isToplevel: Boolean =
    labels.isToplevel

  @inline
  def pop(n: Int): FunctionContext =
    copy(labels = labels.pop(n))

  @inline
  def push(n: Int): FunctionContext =
    copy(labels = labels.push(n))

}

object FunctionContext {
  def apply(arity: Int): FunctionContext =
    FunctionContext(LabelStack(null, 0, arity, 0), Nil, Map.empty, 0, 1)
}

/** Compiles a valid module to the low level [[swam.runtime.internals.interpreter.AsmInst assembly language]].
  * This is a one-pass assembler which generates typed breaks.
  *
  * This compiler executes the static part of the specification semantics, i.e. the one
  * dealing with labels and breaks. The static restrictions on the WebAssembly bytecodes
  * make it possible to statically compute the local stack size at any point in a function
  * body. Of course, it assumes that the module is [[swam.validation.Validator valid]]
  * otherwise it will break or generate undefined assembly code.
  *
  */
class Compiler[F[_]](engine: Engine[F], asm: Asm[F])(implicit F: MonadError[F, Throwable]) {

  private val dataOnHeap = engine.conf.data.onHeap
  private val byteOrder = engine.conf.compiler.byteOrder match {
    case ConfiguredByteOrder.BigEndian    => ByteOrder.BIG_ENDIAN
    case ConfiguredByteOrder.LittleEndian => ByteOrder.LITTLE_ENDIAN
    case ConfiguredByteOrder.Native       => ByteOrder.nativeOrder
  }

  def compile(sections: Stream[F, Section]): Stream[F, runtime.Module[F]] =
    sections
      .fold(Context[F]()) {
        case (ctx, Section.Imports(is)) =>
          val runtimeis = is.map(toRuntime(ctx.types))
          runtimeis.foldLeft(ctx.copy(imports = runtimeis)) {
            case (ctx, runtime.Import.Function(_, _, tpeidx, tpe)) =>
              ctx.copy(funcs = ctx.funcs :+ tpeidx, code = ctx.code :+ compiler.Func.Imported(tpe))
            case (ctx, runtime.Import.Table(_, _, tpe)) =>
              ctx.copy(tables = ctx.tables :+ Tab.Imported(tpe))
            case (ctx, runtime.Import.Memory(_, _, tpe)) =>
              ctx.copy(mems = ctx.mems :+ Mem.Imported(tpe))
            case (ctx, runtime.Import.Global(_, _, tpe)) =>
              ctx.copy(globals = ctx.globals :+ Glob.Imported(tpe))
          }
        case (ctx, Section.Functions(funcs)) =>
          ctx.copy(funcs = ctx.funcs ++ funcs)
        case (ctx, Section.Tables(tables)) =>
          ctx.copy(tables = ctx.tables ++ tables.map(Tab.Compiled(_)))
        case (ctx, Section.Memories(mems)) =>
          ctx.copy(mems = ctx.mems ++ mems.map(Mem.Compiled(_)))
        case (ctx, Section.Globals(globals)) =>
          val cglobals =
            globals.map {
              case Global(tpe, init) =>
                val compiled = compile(0, init, FunctionContext(1), ctx.functions, ctx.types)._1
                Glob.Compiled(CompiledGlobal(tpe, compiled))
            }
          ctx.copy(globals = ctx.globals ++ cglobals)
        case (ctx, Section.Exports(es)) =>
          ctx.copy(exports = es.map(toRuntime(ctx)))
        case (ctx, c @ Section.Custom(name, payload)) =>
          ctx.copy(customs = ctx.customs :+ toRuntime(c))
        case (ctx, Section.Types(types)) =>
          ctx.copy(types = types)
        case (ctx, Section.Code(codes)) =>
          val shift = ctx.funcs.size - codes.size
          val code =
            codes.zipWithIndex.map {
              case ((FuncBody(locals, code)), idx) =>
                val tpe = ctx.types(ctx.funcs(idx + shift))
                val compiled = compile(tpe.params.size + locals.map(_.count).sum,
                                       code,
                                       FunctionContext(tpe.t.size),
                                       ctx.functions,
                                       ctx.types)._1
                val clocals = locals.flatMap(e => Vector.fill(e.count)(e.tpe))
                compiler.Func.Compiled(CompiledFunction(idx + shift, tpe, clocals, compiled))
            }
          ctx.copy(code = code)
        case (ctx, Section.Elements(elems)) =>
          val celems =
            elems.map {
              case Elem(_, offset, init) =>
                val compiled = compile(0, offset, FunctionContext(1), ctx.functions, ctx.types)._1
                CompiledElem(compiled, init)
            }
          ctx.copy(elems = celems)
        case (ctx, Section.Datas(data)) =>
          val cdata =
            data.map {
              case Data(_, offset, bytes) =>
                val compiled = compile(0, offset, FunctionContext(1), ctx.functions, ctx.types)._1
                val dataArray = bytes.toByteArray
                val data =
                  if (dataOnHeap)
                    ByteBuffer.allocate(dataArray.length)
                  else
                    ByteBuffer.allocateDirect(dataArray.length)
                data.order(ByteOrder.LITTLE_ENDIAN)
                data.put(dataArray)
                data.position(0)
                CompiledData(compiled, data)
            }
          ctx.copy(data = cdata)
        case (ctx, Section.Start(idx)) =>
          ctx.copy(start = Some(idx))
      }
      .map { ctx =>
        new runtime.Module(
          ctx.exports,
          ctx.imports,
          ctx.customs,
          ctx.types,
          engine,
          ctx.globals.collect {
            case Glob.Compiled(g) => g
          },
          ctx.tables.collect {
            case Tab.Compiled(t) => t
          },
          ctx.mems.collect {
            case Mem.Compiled(m) => m
          },
          ctx.start,
          ctx.code.collect {
            case compiler.Func.Compiled(f) => f
          },
          ctx.elems,
          ctx.data
        )
      }
      .handleErrorWith(t => Stream.raiseError[F](new CompileException("An error occurred during compilation", t)))

  private def compile(nbLocals: Int,
                      insts: Vector[Inst],
                      ctx: FunctionContext,
                      functions: Vector[FuncType],
                      types: Vector[FuncType]): (Array[AsmInst[F]], FunctionContext) = {

    val builder = ArrayBuilder.make[AsmInst[F]]

    @tailrec
    def loop(instIdx: Int, ctx: FunctionContext, hasReturn: Boolean): (Boolean, FunctionContext) =
      if (instIdx >= insts.size)
        (hasReturn, ctx)
      else
        insts(instIdx) match {
          case Block(tpe, is) =>
            // when entering a new block, push a label with the same
            // arity as the block and a target on break pointing right
            // after the block body.
            // block parameters are considered pushed inside the block
            val nbParams = tpe.params(types).map(_.size).getOrElse(0)
            val ctx1 = ctx.pop(nbParams)
            val label =
              LabelStack(ctx1.labels, ctx1.nxt, tpe.arity(types), nbParams)
            val (compiled, ctx2) =
              compile(nbLocals, is, ctx1.copy(labels = label, nxt = ctx1.nxt + 1), functions, types)
            builder ++= compiled
            // the label target is then the offset right after the body has been compiled
            loop(instIdx + 1,
                 ctx2
                   .copy(labels = ctx1.labels, offsets = ctx2.offsets.updated(ctx1.nxt, ctx2.offset))
                   .push(tpe.arity(types)),
                 false)
          case Loop(tpe, is) =>
            // when entering a new loop, push a label with the same
            // arity as the loop type and a target on break pointing
            // at the loop body start
            // loop parameters are considered pushed inside the loop
            val nbParams = tpe.params(types).map(_.size).getOrElse(0)
            val ctx1 = ctx.pop(nbParams)
            val label = LabelStack(ctx1.labels, ctx1.nxt, nbParams, nbParams)
            val (compiled, ctx2) =
              compile(nbLocals, is, ctx1.copy(labels = label, nxt = ctx1.nxt + 1), functions, types)
            builder ++= compiled
            loop(instIdx + 1,
                 ctx2
                   .copy(labels = ctx1.labels, offsets = ctx2.offsets.updated(ctx1.nxt, ctx1.offset))
                   .push(tpe.arity(types)),
                 false)
          case If(tpe, tis, eis) =>
            // when entering a new if, push a label with the same
            // arity as the if and a target on break pointing right
            // after the end of the if body
            // add a conditional jump to the then part, the else part comes first
            val nbParams = tpe.params(types).map(_.size).getOrElse(0)
            val ctx1 = ctx.pop(nbParams)
            val ctx2 = ctx1.pop(1 /* the if condition */ )
            val label = LabelStack(ctx2.labels, ctx2.nxt, tpe.arity(types), nbParams)
            val thenTarget = ctx2.nxt + 1
            val jumpif = new asm.JumpIf(-1)
            builder += jumpif
            val (ecompiled, ctx3) =
              compile(nbLocals,
                      eis,
                      ctx2.copy(labels = label, nxt = ctx2.nxt + 2, offset = ctx2.offset + 1 /* jumpif */ ),
                      functions,
                      types)
            builder ++= ecompiled
            // jump right after the then part in the end
            val jump = new asm.Jump(-1)
            builder += jump
            val (tcompiled, ctx4) =
              compile(nbLocals, tis, ctx3.copy(offset = ctx3.offset + 1, labels = label), functions, types)
            builder ++= tcompiled
            loop(
              instIdx + 1,
              ctx4
                .copy(
                  labels = ctx2.labels,
                  errata = (thenTarget, jumpif.addr_= _) :: (ctx.nxt, jump.addr_= _) :: ctx4.errata,
                  offsets = ctx4.offsets.updated(thenTarget, ctx3.offset + 1).updated(ctx.nxt, ctx4.offset)
                )
                .push(tpe.arity(types)),
              false
            )
          case Br(lbll) =>
            // break to the given label
            // the instruction parameters are (in order) the arity, the rest to drop, and the target
            val (label, toDrop) = ctx.labels.break(lbll)
            val br = new asm.Br(label.arity, toDrop, -1)
            builder += br
            // save the placeholder offset to be fixed-up with the real label address in the end and skip the rest of the block
            (false, ctx.copy(offset = ctx.offset + 1, errata = (label.target -> br.addr_= _) :: ctx.errata))
          case BrIf(lbll) =>
            // break to the given label if condition is true
            // the instruction parameters are (in order) the arity, the rest to drop, and the target
            val ctx1 = ctx.pop(1 /* the break condition */ )
            val (label, toDrop) = ctx1.labels.break(lbll)
            val brif = new asm.BrIf(label.arity, toDrop, -1)
            builder += brif
            // save the placeholder offset to be fixed-up with the real label address in the end
            loop(instIdx + 1,
                 ctx1.copy(offset = ctx1.offset + 1, errata = (label.target, brif.addr_= _) :: ctx1.errata),
                 false)
          case BrTable(lblls, lbll) =>
            // break to the appropriate label depending on top-level stack value
            val ctx1 = ctx.pop(1 /* the break value */ )
            // the instruction parameters are (in order) `lblls.size`, `lblls.size + 1` times the arity, the rest to drop, and the target
            val (lbls, ctx2) = lblls.foldLeft((ArrayBuilder.make[asm.BrLabel], ctx1)) {
              case ((ab, ctx1), lbll) =>
                val (label, toDrop) = ctx1.labels.break(lbll)
                val brlabel = new asm.BrLabel(label.arity, toDrop, -1)
                // save the placeholder offset to be fixed-up with the real label address in the end
                ab += brlabel
                (ab, ctx1.copy(errata = (label.target, brlabel.addr_= _) :: ctx1.errata))
            }
            val (label, toDrop) = ctx2.labels.break(lbll)
            val dflt = new asm.BrLabel(label.arity, toDrop, -1)
            builder += new asm.BrTable(lbls.result, dflt)
            // save the placeholder offset to be fixed-up with the real label address in the end and skip the rest of the block
            val ctx3 =
              ctx2.copy(offset = ctx2.offset + 1, errata = (label.target, dflt.addr_= _) :: ctx2.errata)
            (false, ctx3)
          case Return =>
            builder += asm.Return
            // skip the rest of the block
            (true, ctx.copy(offset = ctx.offset + 1))
          case op @ Unop(_) =>
            builder += asm.Unop(op)
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case op @ Binop(_) =>
            builder += asm.Binop(op)
            loop(instIdx + 1, ctx.pop(1 /* pop 2 and push 1 */ ).copy(offset = ctx.offset + 1), false)
          case op @ Testop(_) =>
            builder += asm.Testop(op)
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case op @ Relop(_) =>
            builder += asm.Relop(op)
            loop(instIdx + 1, ctx.pop(1 /* pop 2 and push 1 */ ).copy(offset = ctx.offset + 1), false)
          case op @ Convertop(_, _) =>
            builder += asm.Convertop(op)
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case op @ SatConvertop(_, _) =>
            builder += asm.SatConvertop(op)
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case op @ Load(_, _, _) =>
            builder += asm.Load(op)
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case op @ LoadN(_, _, _, _) =>
            builder += asm.LoadN(op)
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case op @ Store(_, _, _) =>
            builder += asm.Store(op)
            loop(instIdx + 1, ctx.pop(2).copy(offset = ctx.offset + 1), false)
          case op @ StoreN(_, _, _, _) =>
            builder += asm.StoreN(op)
            loop(instIdx + 1, ctx.pop(2).copy(offset = ctx.offset + 1), false)
          case MemoryGrow =>
            builder += asm.MemoryGrow
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case MemorySize =>
            builder += asm.MemorySize
            loop(instIdx + 1, ctx.push(1).copy(offset = ctx.offset + 1), false)
          case Drop =>
            builder += new asm.Drop(1)
            loop(instIdx + 1, ctx.pop(1).copy(offset = ctx.offset + 1), false)
          case Select =>
            builder += asm.Select
            loop(instIdx + 1, ctx.pop(2 /* pop 3 push 1 */ ).copy(offset = ctx.offset + 1), false)
          case LocalGet(idx) =>
            builder += new asm.LocalGet(nbLocals - idx)
            loop(instIdx + 1, ctx.push(1).copy(offset = ctx.offset + 1), false)
          case LocalSet(idx) =>
            builder += new asm.LocalSet(nbLocals - idx)
            loop(instIdx + 1, ctx.pop(1).copy(offset = ctx.offset + 1), false)
          case LocalTee(idx) =>
            builder += new asm.LocalTee(nbLocals - idx)
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case GlobalGet(idx) =>
            builder += new asm.GlobalGet(idx)
            loop(instIdx + 1, ctx.push(1).copy(offset = ctx.offset + 1), false)
          case GlobalSet(idx) =>
            builder += new asm.GlobalSet(idx)
            loop(instIdx + 1, ctx.pop(1).copy(offset = ctx.offset + 1), false)
          case Nop =>
            builder += asm.Nop
            loop(instIdx + 1, ctx.copy(offset = ctx.offset + 1), false)
          case Unreachable =>
            builder += asm.Unreachable
            // we can skip the rest of the block
            (false, ctx.copy(offset = ctx.offset + 1))
          case Call(idx) =>
            builder += new asm.Call(idx)
            val tpe = functions(idx)
            loop(instIdx + 1, ctx.pop(tpe.params.size).push(tpe.t.size).copy(offset = ctx.offset + 1), false)
          case CallIndirect(idx) =>
            builder += new asm.CallIndirect(idx)
            val tpe = types(idx)
            loop(instIdx + 1, ctx.pop(1 + tpe.params.size).push(tpe.t.size).copy(offset = ctx.offset + 1), false)
          case i32.Const(v) =>
            builder += new asm.I32Const(v)
            loop(instIdx + 1, ctx.push(1).copy(offset = ctx.offset + 1), false)
          case i64.Const(v) =>
            builder += new asm.I64Const(v)
            loop(instIdx + 1, ctx.push(1).copy(offset = ctx.offset + 1), false)
          case f32.Const(v) =>
            builder += new asm.F32Const(v)
            loop(instIdx + 1, ctx.push(1).copy(offset = ctx.offset + 1), false)
          case f64.Const(v) =>
            builder += new asm.F64Const(v)
            loop(instIdx + 1, ctx.push(1).copy(offset = ctx.offset + 1), false)
        }

    val (hasReturn, ctx1) = loop(0, ctx, false)

    val ctx2 =
      if (ctx.isToplevel) {
        if (!hasReturn) {
          // if this is the top-level and no explicit return has been provided in bytecode, add one
          builder += asm.Return
          ctx1.copy(offsets = ctx1.offsets.updated(0, ctx1.offset))
        } else {
          ctx1.copy(offsets = ctx1.offsets.updated(0, ctx1.offset - 1))
        }
      } else {
        // otherwise go back to the parent label
        ctx1.copy(labels = ctx1.labels.parent)
      }

    val instrs = builder.result()
    if (ctx.isToplevel) {
      // fixup the targets
      for ((target, fix) <- ctx2.errata)
        fix(ctx2.offsets(target))
    }
    (instrs, ctx2)
  }

  private def toRuntime(types: Vector[FuncType])(i: Import): runtime.Import =
    i match {
      case Import.Function(mod, fld, tpe) => runtime.Import.Function(mod, fld, tpe, types(tpe))
      case Import.Table(mod, fld, tpe)    => runtime.Import.Table(mod, fld, tpe)
      case Import.Memory(mod, fld, tpe)   => runtime.Import.Memory(mod, fld, tpe)
      case Import.Global(mod, fld, tpe)   => runtime.Import.Global(mod, fld, tpe)
    }

  private def toRuntime(ctx: Context[F])(e: Export): runtime.Export =
    e match {
      case Export(fld, ExternalKind.Function, idx) => runtime.Export.Function(fld, ctx.types(ctx.funcs(idx)), idx)
      case Export(fld, ExternalKind.Table, idx)    => runtime.Export.Table(fld, ctx.tables(idx).tpe, idx)
      case Export(fld, ExternalKind.Memory, idx)   => runtime.Export.Memory(fld, ctx.mems(idx).tpe, idx)
      case Export(fld, ExternalKind.Global, idx)   => runtime.Export.Global(fld, ctx.globals(idx).tpe, idx)
    }

  private def toRuntime(c: Section.Custom): runtime.Custom =
    runtime.Custom(c.name, c.payload)

}
