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
import store._

import cats._

import fs2._

import scodec._

import scala.collection.mutable.ArrayBuilder

import scala.language.higherKinds

case class Context(types: Vector[FuncType] = Vector.empty,
                   funcs: Vector[Int] = Vector.empty,
                   code: Vector[Int] = Vector.empty,
                   tables: Vector[TableType] = Vector.empty,
                   mems: Vector[MemType] = Vector.empty,
                   globals: Vector[GlobalType] = Vector.empty,
                   elems: Vector[(Int, Vector[Int])] = Vector.empty,
                   data: Vector[(Int, Vector[Byte])] = Vector.empty,
                   start: Option[Int] = None,
                   exports: Vector[runtime.Export] = Vector.empty,
                   imports: Vector[runtime.Import] = Vector.empty,
                   customs: Vector[runtime.Custom] = Vector.empty)

/** Validates and compiles a module.
  */
class Compiler[F[_]](engine: SwamEngine[F])(implicit F: MonadError[F, Throwable]) {

  def compile(sections: Stream[F, Section]): Stream[F, runtime.Module[F]] =
    sections
      .fold(new Context()) {
        case (ctx, Section.Imports(is)) =>
          val runtimeis = is.map(toRuntime(ctx.types))
          runtimeis.foldLeft(ctx.copy(imports = runtimeis)) {
            case (ctx, runtime.Import.Function(_, _, tpe, _)) => ctx.copy(funcs = ctx.funcs :+ tpe)
            case (ctx, runtime.Import.Global(_, _, tpe))      => ctx.copy(globals = ctx.globals :+ tpe)
            case (ctx, runtime.Import.Memory(_, _, tpe))      => ctx.copy(mems = ctx.mems :+ tpe)
            case (ctx, runtime.Import.Table(_, _, tpe))       => ctx.copy(tables = ctx.tables :+ tpe)
          }
        case (ctx, Section.Functions(funcs)) =>
          ctx.copy(funcs = ctx.funcs ++ funcs)
        case (ctx, Section.Tables(tables)) =>
          ctx.copy(tables = ctx.tables ++ tables)
        case (ctx, Section.Memories(mems)) =>
          ctx.copy(mems = ctx.mems ++ mems)
        case (ctx, Section.Globals(globals)) =>
          ctx.copy(globals = ctx.globals ++ globals.map(_.tpe))
        case (ctx, Section.Exports(es)) =>
          ctx.copy(exports = es.map(toRuntime(ctx)))
        case (ctx, c @ Section.Custom(_, _)) =>
          ctx.copy(customs = ctx.customs :+ toRuntime(c))
        case (ctx, Section.Types(types)) =>
          ctx.copy(types = types)
        case (ctx, Section.Code(codes)) =>
          val funcAddresses =
            codes.map {
              case (FuncBody(locals, code)) =>
                val bytes = compile(code, true)
                val addr = engine.store.static.allocate(bytes.length)
                engine.store.static.writeBytes(addr, bytes)
                addr
            }
          ctx.copy(code = funcAddresses)
        case (ctx, Section.Elements(elems)) =>
          val compiledElems =
            elems.map {
              case Elem(_, offset, init) =>
                val offsetAddress = {
                  val bytes = compile(offset, true)
                  val addr = engine.store.static.allocate(bytes.length)
                  engine.store.static.writeBytes(addr, bytes)
                  addr
                }
                // get the function addresses from indices
                val faddrs = init.map(ctx.code(_))
                (offsetAddress, faddrs)
            }
          ctx.copy(elems = compiledElems)
        case (ctx, Section.Datas(data)) =>
          val compileData =
            data.map {
              case Data(_, offset, bytes) =>
                val offsetAddress = {
                  val bytes = compile(offset, true)
                  val addr = engine.store.static.allocate(bytes.length)
                  engine.store.static.writeBytes(addr, bytes)
                  addr
                }
                (offsetAddress, bytes.toByteArray.toVector)
            }
          ctx.copy(data = compileData)
        case (ctx, Section.Start(idx)) =>
          ctx.copy(start = Some(ctx.funcs(idx)))
      }
      .evalMap { ctx =>
        val tablemin = ctx.tables.headOption.map(_.limits.min).getOrElse(-1)
        val tablemax = ctx.tables.headOption.flatMap(_.limits.max).getOrElse(-1)
        val memmin = ctx.mems.headOption.map(_.limits.min).getOrElse(-1)
        val memmax = ctx.mems.headOption.flatMap(_.limits.max).getOrElse(-1)
        val start = ctx.start.getOrElse(-1)
        val globals = ctx.globals.map(gt => type2byte(gt.tpe))
        val stored = StoredModule(tablemin, tablemax, memmin, memmax, start, globals, ctx.code, ctx.elems, ctx.data)
        StoredModule.codec.encode(stored) match {
          case Attempt.Successful(bytes) =>
            val bytea = bytes.toByteArray
            val address = engine.store.static.allocate(bytea.length)
            engine.store.static.writeBytes(address, bytea)
            F.pure(
              new runtime.Module(ctx.exports,
                                 ctx.imports,
                                 ctx.customs,
                                 ctx.types,
                                 engine,
                                 address,
                                 20 + ctx.globals.size * 4))
          case Attempt.Failure(f) =>
            F.raiseError[runtime.Module[F]](new MemoryException(f.message))
        }
      }

  private def compile(insts: Vector[Inst], toplevel: Boolean): Array[Byte] = {
    val (builder, hasReturn) =
      insts.foldLeft((ArrayBuilder.make[Byte], false)) {
        case ((builder, _), op @ MemoryInst(offset, align)) =>
          builder += op.opcode.toByte
          storeInt(builder, offset)
          storeInt(builder, align)
          (builder, false)
        case ((builder, _), op @ VarInst(idx)) =>
          builder += op.opcode.toByte
          storeInt(builder, idx)
          (builder, false)
        case ((builder, _), op @ Block(tpe, instrs)) =>
          val inner = compile(instrs, false)
          val arity = tpe.t match {
            case None    => 0
            case Some(_) => 1
          }
          builder += op.opcode.toByte
          storeInt(builder, arity)
          storeInt(builder, inner.size)
          builder ++= inner
          builder += OpCode.End.toByte
          (builder, false)
        case ((builder, _), op @ Loop(tpe, instrs)) =>
          val inner = compile(instrs, false)
          val arity = tpe.t match {
            case None    => 0
            case Some(_) => 1
          }
          builder += op.opcode.toByte
          storeInt(builder, arity)
          builder ++= inner
          builder += OpCode.End.toByte
          (builder, false)
        case ((builder, _), op @ If(tpe, t, e)) =>
          val thenInstr = compile(t, false)
          val elseInstr = compile(e, false)
          val arity = tpe.t match {
            case None    => 0
            case Some(_) => 1
          }
          builder += op.opcode.toByte
          storeInt(builder, arity)
          storeInt(builder, thenInstr.size)
          storeInt(builder, elseInstr.size)
          builder ++= thenInstr
          builder += OpCode.Else.toByte
          storeInt(builder, elseInstr.size)
          builder ++= elseInstr
          builder += OpCode.End.toByte
          (builder, false)
        case ((builder, _), op @ Br(lbl)) =>
          builder += op.opcode.toByte
          storeInt(builder, lbl)
          (builder, false)
        case ((builder, _), op @ BrIf(lbl)) =>
          builder += op.opcode.toByte
          storeInt(builder, lbl)
          (builder, false)
        case ((builder, _), op @ BrTable(lbls, lbl)) =>
          builder += op.opcode.toByte
          storeInt(builder, lbls.size)
          for (l <- lbls)
            storeInt(builder, l)
          storeInt(builder, lbl)
          (builder, false)
        case ((builder, _), op @ Call(idx)) =>
          builder += op.opcode.toByte
          storeInt(builder, idx)
          (builder, false)
        case ((builder, _), op @ CallIndirect(idx)) =>
          builder += op.opcode.toByte
          storeInt(builder, idx)
          (builder, false)
        case ((builder, _), op @ Return) =>
          builder += op.opcode.toByte
          (builder, true)
        case ((builder, _), op) =>
          builder += op.opcode.toByte
          (builder, false)
      }
    if (toplevel && !hasReturn)
      builder += OpCode.Return.toByte
    builder.result()
  }

  private def type2byte(tpe: ValType): Byte =
    tpe match {
      case ValType.I32 => 0x7f
      case ValType.I64 => 0x7e
      case ValType.F32 => 0x7d
      case ValType.F64 => 0x7c
    }

  private def storeInt(builder: ArrayBuilder[Byte], i: Int): ArrayBuilder[Byte] = {
    // store integers in little-endian
    builder += (i & 0xff).toByte
    builder += ((i >> 8) & 0xff).toByte
    builder += ((i >> 16) & 0xff).toByte
    builder += ((i >> 24) & 0xff).toByte
  }

  private def toRuntime(types: Vector[FuncType])(i: Import): runtime.Import =
    i match {
      case Import.Function(mod, fld, tpe) => runtime.Import.Function(mod, fld, tpe, types(tpe))
      case Import.Table(mod, fld, tpe)    => runtime.Import.Table(mod, fld, tpe)
      case Import.Memory(mod, fld, tpe)   => runtime.Import.Memory(mod, fld, tpe)
      case Import.Global(mod, fld, tpe)   => runtime.Import.Global(mod, fld, tpe)
    }

  private def toRuntime(ctx: Context)(e: Export): runtime.Export =
    e match {
      case Export(fld, ExternalKind.Function, idx) => runtime.Export.Function(fld, ctx.types(ctx.funcs(idx)))
      case Export(fld, ExternalKind.Table, idx)    => runtime.Export.Table(fld, ctx.tables(idx))
      case Export(fld, ExternalKind.Memory, idx)   => runtime.Export.Memory(fld, ctx.mems(idx))
      case Export(fld, ExternalKind.Global, idx)   => runtime.Export.Global(fld, ctx.globals(idx))
    }

  private def toRuntime(c: Section.Custom): runtime.Custom =
    runtime.Custom(c.name, c.payload)

}
