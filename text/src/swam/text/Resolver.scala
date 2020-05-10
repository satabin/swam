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
package text

import unresolved._
import swam.{syntax => r}
import binary.custom._

import cats.MonadError
import cats.implicits._

import fs2.Stream

import scodec._
import scodec.bits._

import scala.annotation.tailrec

import scala.collection.immutable.VectorBuilder

/** Implements text module resolution as per the specification.
  *  All indices and names are checked for existence and replaced by the index in
  *  the loaded module.
  */
class Resolver[F[_]](implicit F: MonadError[F, Throwable]) {

  private def resolveAll(instrs: Seq[Inst], ctx: ResolverContext): F[(Vector[r.Inst], ResolverContext)] =
    F.tailRecM((instrs, ctx, new VectorBuilder[r.Inst])) {
      case (Seq(), ctx, acc) => F.pure(Right((acc.result, ctx)))
      case (Seq(i, rest @ _*), ctx, acc) =>
        resolve(i, ctx).map { case (i, ctx) => Left((rest, ctx, acc += i)) }
    }

  /** Resolves an instruction.
    *  The references to all declared elements are replaced by their index
    *  in the module.
    */
  def resolve(instr: Inst, ctx: ResolverContext): F[(r.Inst, ResolverContext)] =
    instr match {
      case i32.Const(v: Int)          => F.pure((r.i32.Const(v), ctx))
      case i32.Clz()                  => F.pure((r.i32.Clz, ctx))
      case i32.Ctz()                  => F.pure((r.i32.Ctz, ctx))
      case i32.Popcnt()               => F.pure((r.i32.Popcnt, ctx))
      case i32.Extend8S()             => F.pure((r.i32.Extend8S, ctx))
      case i32.Extend16S()            => F.pure((r.i32.Extend16S, ctx))
      case i32.Add()                  => F.pure((r.i32.Add, ctx))
      case i32.Sub()                  => F.pure((r.i32.Sub, ctx))
      case i32.Mul()                  => F.pure((r.i32.Mul, ctx))
      case i32.DivS()                 => F.pure((r.i32.DivS, ctx))
      case i32.DivU()                 => F.pure((r.i32.DivU, ctx))
      case i32.RemS()                 => F.pure((r.i32.RemS, ctx))
      case i32.RemU()                 => F.pure((r.i32.RemU, ctx))
      case i32.And()                  => F.pure((r.i32.And, ctx))
      case i32.Or()                   => F.pure((r.i32.Or, ctx))
      case i32.Xor()                  => F.pure((r.i32.Xor, ctx))
      case i32.Shl()                  => F.pure((r.i32.Shl, ctx))
      case i32.ShrS()                 => F.pure((r.i32.ShrS, ctx))
      case i32.ShrU()                 => F.pure((r.i32.ShrU, ctx))
      case i32.Rotl()                 => F.pure((r.i32.Rotl, ctx))
      case i32.Rotr()                 => F.pure((r.i32.Rotr, ctx))
      case i32.Eqz()                  => F.pure((r.i32.Eqz, ctx))
      case i32.Eq()                   => F.pure((r.i32.Eq, ctx))
      case i32.Ne()                   => F.pure((r.i32.Ne, ctx))
      case i32.LtS()                  => F.pure((r.i32.LtS, ctx))
      case i32.LtU()                  => F.pure((r.i32.LtU, ctx))
      case i32.GtS()                  => F.pure((r.i32.GtS, ctx))
      case i32.GtU()                  => F.pure((r.i32.GtU, ctx))
      case i32.LeS()                  => F.pure((r.i32.LeS, ctx))
      case i32.LeU()                  => F.pure((r.i32.LeU, ctx))
      case i32.GeS()                  => F.pure((r.i32.GeS, ctx))
      case i32.GeU()                  => F.pure((r.i32.GeU, ctx))
      case i32.WrapI64()              => F.pure((r.i32.WrapI64, ctx))
      case i32.TruncSF32()            => F.pure((r.i32.TruncSF32, ctx))
      case i32.TruncUF32()            => F.pure((r.i32.TruncUF32, ctx))
      case i32.TruncSF64()            => F.pure((r.i32.TruncSF64, ctx))
      case i32.TruncUF64()            => F.pure((r.i32.TruncUF64, ctx))
      case i32.TruncSatSF32()         => F.pure((r.i32.TruncSatSF32, ctx))
      case i32.TruncSatUF32()         => F.pure((r.i32.TruncSatUF32, ctx))
      case i32.TruncSatSF64()         => F.pure((r.i32.TruncSatSF64, ctx))
      case i32.TruncSatUF64()         => F.pure((r.i32.TruncSatUF64, ctx))
      case i32.ReinterpretF32()       => F.pure((r.i32.ReinterpretF32, ctx))
      case i32.Load(align, offset)    => F.pure((r.i32.Load(align, offset), ctx))
      case i32.Store(align, offset)   => F.pure((r.i32.Store(align, offset), ctx))
      case i32.Load8S(align, offset)  => F.pure((r.i32.Load8S(align, offset), ctx))
      case i32.Load8U(align, offset)  => F.pure((r.i32.Load8U(align, offset), ctx))
      case i32.Load16S(align, offset) => F.pure((r.i32.Load16S(align, offset), ctx))
      case i32.Load16U(align, offset) => F.pure((r.i32.Load16U(align, offset), ctx))
      case i32.Store8(align, offset)  => F.pure((r.i32.Store8(align, offset), ctx))
      case i32.Store16(align, offset) => F.pure((r.i32.Store16(align, offset), ctx))
      case i64.Const(v)               => F.pure((r.i64.Const(v), ctx))
      case i64.Clz()                  => F.pure((r.i64.Clz, ctx))
      case i64.Ctz()                  => F.pure((r.i64.Ctz, ctx))
      case i64.Popcnt()               => F.pure((r.i64.Popcnt, ctx))
      case i64.Extend8S()             => F.pure((r.i64.Extend8S, ctx))
      case i64.Extend16S()            => F.pure((r.i64.Extend16S, ctx))
      case i64.Extend32S()            => F.pure((r.i64.Extend32S, ctx))
      case i64.Add()                  => F.pure((r.i64.Add, ctx))
      case i64.Sub()                  => F.pure((r.i64.Sub, ctx))
      case i64.Mul()                  => F.pure((r.i64.Mul, ctx))
      case i64.DivS()                 => F.pure((r.i64.DivS, ctx))
      case i64.DivU()                 => F.pure((r.i64.DivU, ctx))
      case i64.RemS()                 => F.pure((r.i64.RemS, ctx))
      case i64.RemU()                 => F.pure((r.i64.RemU, ctx))
      case i64.And()                  => F.pure((r.i64.And, ctx))
      case i64.Or()                   => F.pure((r.i64.Or, ctx))
      case i64.Xor()                  => F.pure((r.i64.Xor, ctx))
      case i64.Shl()                  => F.pure((r.i64.Shl, ctx))
      case i64.ShrS()                 => F.pure((r.i64.ShrS, ctx))
      case i64.ShrU()                 => F.pure((r.i64.ShrU, ctx))
      case i64.Rotl()                 => F.pure((r.i64.Rotl, ctx))
      case i64.Rotr()                 => F.pure((r.i64.Rotr, ctx))
      case i64.Eqz()                  => F.pure((r.i64.Eqz, ctx))
      case i64.Eq()                   => F.pure((r.i64.Eq, ctx))
      case i64.Ne()                   => F.pure((r.i64.Ne, ctx))
      case i64.LtS()                  => F.pure((r.i64.LtS, ctx))
      case i64.LtU()                  => F.pure((r.i64.LtU, ctx))
      case i64.GtS()                  => F.pure((r.i64.GtS, ctx))
      case i64.GtU()                  => F.pure((r.i64.GtU, ctx))
      case i64.LeS()                  => F.pure((r.i64.LeS, ctx))
      case i64.LeU()                  => F.pure((r.i64.LeU, ctx))
      case i64.GeS()                  => F.pure((r.i64.GeS, ctx))
      case i64.GeU()                  => F.pure((r.i64.GeU, ctx))
      case i64.ExtendSI32()           => F.pure((r.i64.ExtendSI32, ctx))
      case i64.ExtendUI32()           => F.pure((r.i64.ExtendUI32, ctx))
      case i64.TruncSF32()            => F.pure((r.i64.TruncSF32, ctx))
      case i64.TruncUF32()            => F.pure((r.i64.TruncUF32, ctx))
      case i64.TruncSF64()            => F.pure((r.i64.TruncSF64, ctx))
      case i64.TruncUF64()            => F.pure((r.i64.TruncUF64, ctx))
      case i64.TruncSatSF32()         => F.pure((r.i64.TruncSatSF32, ctx))
      case i64.TruncSatUF32()         => F.pure((r.i64.TruncSatUF32, ctx))
      case i64.TruncSatSF64()         => F.pure((r.i64.TruncSatSF64, ctx))
      case i64.TruncSatUF64()         => F.pure((r.i64.TruncSatUF64, ctx))
      case i64.ReinterpretF64()       => F.pure((r.i64.ReinterpretF64, ctx))
      case i64.Load(align, offset)    => F.pure((r.i64.Load(align, offset), ctx))
      case i64.Store(align, offset)   => F.pure((r.i64.Store(align, offset), ctx))
      case i64.Load8S(align, offset)  => F.pure((r.i64.Load8S(align, offset), ctx))
      case i64.Load8U(align, offset)  => F.pure((r.i64.Load8U(align, offset), ctx))
      case i64.Load16S(align, offset) => F.pure((r.i64.Load16S(align, offset), ctx))
      case i64.Load16U(align, offset) => F.pure((r.i64.Load16U(align, offset), ctx))
      case i64.Load32S(align, offset) => F.pure((r.i64.Load32S(align, offset), ctx))
      case i64.Load32U(align, offset) => F.pure((r.i64.Load32U(align, offset), ctx))
      case i64.Store8(align, offset)  => F.pure((r.i64.Store8(align, offset), ctx))
      case i64.Store16(align, offset) => F.pure((r.i64.Store16(align, offset), ctx))
      case i64.Store32(align, offset) => F.pure((r.i64.Store32(align, offset), ctx))
      case f32.Const(v)               => F.pure((r.f32.Const(v), ctx))
      case f32.Abs()                  => F.pure((r.f32.Abs, ctx))
      case f32.Neg()                  => F.pure((r.f32.Neg, ctx))
      case f32.Sqrt()                 => F.pure((r.f32.Sqrt, ctx))
      case f32.Ceil()                 => F.pure((r.f32.Ceil, ctx))
      case f32.Floor()                => F.pure((r.f32.Floor, ctx))
      case f32.Trunc()                => F.pure((r.f32.Trunc, ctx))
      case f32.Nearest()              => F.pure((r.f32.Nearest, ctx))
      case f32.Add()                  => F.pure((r.f32.Add, ctx))
      case f32.Sub()                  => F.pure((r.f32.Sub, ctx))
      case f32.Mul()                  => F.pure((r.f32.Mul, ctx))
      case f32.Div()                  => F.pure((r.f32.Div, ctx))
      case f32.Min()                  => F.pure((r.f32.Min, ctx))
      case f32.Max()                  => F.pure((r.f32.Max, ctx))
      case f32.Copysign()             => F.pure((r.f32.Copysign, ctx))
      case f32.Eq()                   => F.pure((r.f32.Eq, ctx))
      case f32.Ne()                   => F.pure((r.f32.Ne, ctx))
      case f32.Lt()                   => F.pure((r.f32.Lt, ctx))
      case f32.Gt()                   => F.pure((r.f32.Gt, ctx))
      case f32.Le()                   => F.pure((r.f32.Le, ctx))
      case f32.Ge()                   => F.pure((r.f32.Ge, ctx))
      case f32.DemoteF64()            => F.pure((r.f32.DemoteF64, ctx))
      case f32.ConvertSI32()          => F.pure((r.f32.ConvertSI32, ctx))
      case f32.ConvertUI32()          => F.pure((r.f32.ConvertUI32, ctx))
      case f32.ConvertSI64()          => F.pure((r.f32.ConvertSI64, ctx))
      case f32.ConvertUI64()          => F.pure((r.f32.ConvertUI64, ctx))
      case f32.ReinterpretI32()       => F.pure((r.f32.ReinterpretI32, ctx))
      case f32.Load(align, offset)    => F.pure((r.f32.Load(align, offset), ctx))
      case f32.Store(align, offset)   => F.pure((r.f32.Store(align, offset), ctx))
      case f64.Const(v)               => F.pure((r.f64.Const(v), ctx))
      case f64.Abs()                  => F.pure((r.f64.Abs, ctx))
      case f64.Neg()                  => F.pure((r.f64.Neg, ctx))
      case f64.Sqrt()                 => F.pure((r.f64.Sqrt, ctx))
      case f64.Ceil()                 => F.pure((r.f64.Ceil, ctx))
      case f64.Floor()                => F.pure((r.f64.Floor, ctx))
      case f64.Trunc()                => F.pure((r.f64.Trunc, ctx))
      case f64.Nearest()              => F.pure((r.f64.Nearest, ctx))
      case f64.Add()                  => F.pure((r.f64.Add, ctx))
      case f64.Sub()                  => F.pure((r.f64.Sub, ctx))
      case f64.Mul()                  => F.pure((r.f64.Mul, ctx))
      case f64.Div()                  => F.pure((r.f64.Div, ctx))
      case f64.Min()                  => F.pure((r.f64.Min, ctx))
      case f64.Max()                  => F.pure((r.f64.Max, ctx))
      case f64.Copysign()             => F.pure((r.f64.Copysign, ctx))
      case f64.Eq()                   => F.pure((r.f64.Eq, ctx))
      case f64.Ne()                   => F.pure((r.f64.Ne, ctx))
      case f64.Lt()                   => F.pure((r.f64.Lt, ctx))
      case f64.Gt()                   => F.pure((r.f64.Gt, ctx))
      case f64.Le()                   => F.pure((r.f64.Le, ctx))
      case f64.Ge()                   => F.pure((r.f64.Ge, ctx))
      case f64.PromoteF32()           => F.pure((r.f64.PromoteF32, ctx))
      case f64.ConvertSI32()          => F.pure((r.f64.ConvertSI32, ctx))
      case f64.ConvertUI32()          => F.pure((r.f64.ConvertUI32, ctx))
      case f64.ConvertSI64()          => F.pure((r.f64.ConvertSI64, ctx))
      case f64.ConvertUI64()          => F.pure((r.f64.ConvertUI64, ctx))
      case f64.ReinterpretI64()       => F.pure((r.f64.ReinterpretI64, ctx))
      case f64.Load(align, offset)    => F.pure((r.f64.Load(align, offset), ctx))
      case f64.Store(align, offset)   => F.pure((r.f64.Store(align, offset), ctx))
      case Drop()                     => F.pure((r.Drop, ctx))
      case Select()                   => F.pure((r.Select, ctx))
      case LocalGet(idx)              => resolveIndex(idx, instr.pos, ctx.locals, "local").map(idx => (r.LocalGet(idx), ctx))
      case LocalSet(idx)              => resolveIndex(idx, instr.pos, ctx.locals, "local").map(idx => (r.LocalSet(idx), ctx))
      case LocalTee(idx)              => resolveIndex(idx, instr.pos, ctx.locals, "local").map(idx => (r.LocalTee(idx), ctx))
      case GlobalGet(idx)             => resolveIndex(idx, instr.pos, ctx.globals, "global").map(idx => (r.GlobalGet(idx), ctx))
      case GlobalSet(idx)             => resolveIndex(idx, instr.pos, ctx.globals, "global").map(idx => (r.GlobalSet(idx), ctx))
      case MemorySize()               => F.pure((r.MemorySize, ctx))
      case MemoryGrow()               => F.pure((r.MemoryGrow, ctx))
      case Nop()                      => F.pure((r.Nop, ctx))
      case Unreachable()              => F.pure((r.Unreachable, ctx))
      case Block(label, TypeUse(tpe, params, result), is, endlabel) =>
        resolveTypeUse(tpe, params, result, ctx, instr.pos).flatMap {
          case (idx, ctx1) =>
            resolveBlock(label,
                         BlockType.FunctionType(idx),
                         is,
                         endlabel,
                         ctx.copy(typedefs = ctx.typedefs ++ ctx1.typedefs),
                         instr.pos)
        }
      case Loop(label, tu @ TypeUse(tpe, params, result), is, endlabel) =>
        resolveTypeUse(tpe, params, result, ctx, instr.pos).flatMap {
          case (idx, ctx1) =>
            resolveLoop(label,
                        BlockType.FunctionType(idx),
                        is,
                        endlabel,
                        ctx.copy(typedefs = ctx.typedefs ++ ctx1.typedefs),
                        instr.pos)
        }
      case If(label, TypeUse(tpe, params, result), theni, elselabel, elsei, endlabel) =>
        resolveTypeUse(tpe, params, result, ctx, instr.pos).flatMap {
          case (idx, ctx1) =>
            resolveIf(label,
                      BlockType.FunctionType(idx),
                      theni,
                      elselabel,
                      elsei,
                      endlabel,
                      ctx.copy(typedefs = ctx.typedefs ++ ctx1.typedefs),
                      instr.pos)
        }
      case Br(label)                                   => resolveIndex(label, instr.pos, ctx.labels, "label").map(idx => (r.Br(idx), ctx))
      case BrIf(label)                                 => resolveIndex(label, instr.pos, ctx.labels, "label").map(idx => (r.BrIf(idx), ctx))
      case BrTable(table, lbl)                         => resolveBrTable(table, lbl, ctx, instr.pos)
      case Return()                                    => F.pure((r.Return, ctx))
      case Call(lbl)                                   => resolveCall(lbl, ctx, instr.pos)
      case CallIndirect(TypeUse(tpe, params, results)) => resolveCallIndirect(tpe, params, results, ctx, instr.pos)
    }

  private def resolveIndices(idx: Seq[Index], pos: Int, vars: Vector[Def], kind: String)(
      implicit F: MonadError[F, Throwable]): F[Vector[Int]] =
    F.tailRecM((idx, new VectorBuilder[Int])) {
      case (Seq(), acc) => F.pure(Right(acc.result))
      case (Seq(idx, rest @ _*), acc) =>
        resolveIndex(idx, pos, vars, kind).map(idx => Left((rest, acc += idx)))
    }

  private def resolveIndex(idx: Index, pos: Int, vars: Vector[Def], kind: String)(
      implicit F: MonadError[F, Throwable]): F[Int] =
    idx match {
      case Left(idx) =>
        F.pure(idx)
      case Right(id) =>
        val idx = vars.indexWhere(_.id == id)
        if (idx >= 0)
          F.pure(idx)
        else
          F.raiseError(new ResolutionException(f"Unknown $kind with name $id", Seq(pos)))
    }

  private def resolveBlock(label: Id, tpe: BlockType, is: Seq[Inst], endlabel: Id, ctx: ResolverContext, pos: Int)(
      implicit F: MonadError[F, Throwable]): F[(r.Inst, ResolverContext)] =
    checkLabels(label, endlabel, pos).flatMap { _ =>
      resolveAll(is, ctx.pushLabel(label, pos)).map {
        case (is, ctx) => (r.Block(tpe, is), ctx.popLabel)
      }
    }

  private def resolveLoop(label: Id, tpe: BlockType, is: Seq[Inst], endlabel: Id, ctx: ResolverContext, pos: Int)(
      implicit F: MonadError[F, Throwable]): F[(r.Inst, ResolverContext)] =
    checkLabels(label, endlabel, pos).flatMap { _ =>
      resolveAll(is, ctx.pushLabel(label, pos)).map {
        case (is, ctx) => (r.Loop(tpe, is), ctx.popLabel)
      }
    }

  private def resolveIf(label: Id,
                        tpe: BlockType,
                        theni: Seq[Inst],
                        elselabel: Id,
                        elsei: Seq[Inst],
                        endlabel: Id,
                        ctx: ResolverContext,
                        pos: Int): F[(r.Inst, ResolverContext)] =
    checkLabels(label, elselabel, pos).flatMap { _ =>
      checkLabels(label, endlabel, pos).flatMap { _ =>
        resolveAll(theni, ctx.pushLabel(label, pos)).flatMap {
          case (theni, ctx) =>
            resolveAll(elsei, ctx).map {
              case (elsei, ctx) =>
                (r.If(tpe, theni, elsei), ctx.popLabel)
            }
        }
      }
    }

  private def resolveBrTable(table: Vector[Index], lbl: Index, ctx: ResolverContext, pos: Int)(
      implicit F: MonadError[F, Throwable]): F[(r.Inst, ResolverContext)] =
    resolveIndices(table, pos, ctx.labels, "labels").flatMap { table =>
      resolveIndex(lbl, pos, ctx.labels, "labels").map { lbl => (r.BrTable(table, lbl), ctx) }
    }

  private def resolveCall(lbl: Index, ctx: ResolverContext, pos: Int)(
      implicit F: MonadError[F, Throwable]): F[(r.Inst, ResolverContext)] =
    resolveIndex(lbl, pos, ctx.funcs, "function").map { idx => (r.Call(idx), ctx) }

  private def resolveCallIndirect(tpe: Option[Index],
                                  params: Vector[Param],
                                  results: Vector[ValType],
                                  ctx: ResolverContext,
                                  pos: Int): F[(r.Inst, ResolverContext)] =
    resolveTypeUse(tpe, params, results, ctx, pos).flatMap {
      case (idx, ctx1) =>
        if (ctx1.locals.forall(_.id == NoId))
          F.pure((r.CallIndirect(idx), ctx.copy(typedefs = ctx.typedefs ++ ctx1.typedefs)))
        else
          F.raiseError(
            new ResolutionException(f"Parameters are not allowed to be named in call indirect: ${ctx1.locals}",
                                    Seq(pos)))
    }

  private def resolveTypeUse(tpe: Option[Index],
                             params: Vector[Param],
                             results: Vector[ValType],
                             ctx: ResolverContext,
                             pos: Int): F[(Int, ResolverContext)] = {
    val (nparams, tparams) = params.unzip
    if (distinctNames(nparams))
      tpe match {
        case Some(tpe) =>
          resolveIndex(tpe, pos, ctx.types, "type").flatMap { idx =>
            if (idx >= 0 && idx < ctx.typedefs.size)
              if (params.isEmpty && results.isEmpty)
                F.pure((idx, ResolverContext(locals = Vector.fill(ctx.typedefs(idx).params.size)(Def(NoId, pos)))))
              else
                ctx.typedefs(idx) match {
                  case FuncType(`tparams`, `results`) =>
                    F.pure((idx, ResolverContext(locals = nparams.map(Def(_, pos)))))
                  case _ =>
                    F.raiseError(new ResolutionException(f"Incompatible types", Seq(pos)))
                }
            else
              F.raiseError(new ResolutionException(f"Unknown type $idx", Seq(pos)))
          }
        case None =>
          val functype = FuncType(tparams, results)
          val idx = ctx.typedefs.indexOf(functype)
          if (idx >= 0)
            F.pure((idx, ResolverContext(locals = nparams.map(Def(_, pos)))))
          else
            F.pure((ctx.typedefs.size, ResolverContext(locals = nparams.map(Def(_, pos)), typedefs = Vector(functype))))
      }
    else {
      F.raiseError(new ResolutionException(f"Duplicate parameter names", Seq(pos)))
    }
  }

  private def checkLabels(start: Id, end: Id, pos: Int): F[Unit] =
    (start, end) match {
      case (SomeId(s), SomeId(e)) if s != e =>
        F.raiseError(new ResolutionException(f"Start and end labels must match", Seq(pos)))
      case _ => F.pure(())
    }

  private def distinctNames(names: Vector[Id]): Boolean = {
    @tailrec
    def loop(idx: Int, seen: Set[Id]): Boolean =
      if (idx >= names.size)
        true
      else
        names(idx) match {
          case NoId =>
            loop(idx + 1, seen)
          case id =>
            if (seen.contains(id))
              false
            else
              loop(idx + 1, seen + id)
        }
    loop(0, Set.empty)
  }

  /** Resolves the entire module, stopping on the first encountered error. */
  def resolve(mod: Module, debug: Boolean = false): F[Stream[F, r.Section]] = {
    // first we initialize the resolver context with declared identiiers
    // a variation from the spec is that fresh identifiers are not sytactically correct
    // (they do not start with a "$"). This ensures that they indeed are fresh
    val ctx = mod.fields.foldLeft(ResolverContext(name = mod.id)) {
      case (ctx, fld @ Type(id, _, tpe)) =>
        ctx.copy(types = ctx.types :+ Def(id, fld.pos), typedefs = ctx.typedefs :+ tpe)
      case (ctx, fld @ Function(id, _, _, _)) =>
        ctx.copy(funcs = ctx.funcs :+ Def(id, fld.pos))
      case (ctx, fld @ Table(id, _)) =>
        ctx.copy(tables = ctx.tables :+ Def(id, fld.pos))
      case (ctx, fld @ Memory(id, _)) =>
        ctx.copy(mems = ctx.mems :+ Def(id, fld.pos))
      case (ctx, fld @ Global(id, _, _)) =>
        ctx.copy(globals = ctx.globals :+ Def(id, fld.pos))
      case (ctx, fld @ Import(_, _, ImportDesc.Func(id, _))) =>
        ctx.copy(funcs = ctx.funcs :+ Def(id, fld.pos))
      case (ctx, fld @ Import(_, _, ImportDesc.Table(id, _))) =>
        ctx.copy(tables = ctx.tables :+ Def(id, fld.pos))
      case (ctx, fld @ Import(_, _, ImportDesc.Memory(id, _))) =>
        ctx.copy(mems = ctx.mems :+ Def(id, fld.pos))
      case (ctx, fld @ Import(_, _, ImportDesc.Global(id, _))) =>
        ctx.copy(globals = ctx.globals :+ Def(id, fld.pos))
      case (ctx, _) => ctx
    }

    checkWellFormed(ctx).flatMap { _ =>
      F.tailRecM((ctx, mod.fields, r.Module.empty)) {
        case (ctx, Seq(), resolved) =>
          // add potential new functypes
          val resolved1 =
            if (ctx.types.size < ctx.typedefs.size)
              resolved.copy(types = resolved.types ++ ctx.typedefs.drop(ctx.types.size))
            else
              resolved
          F.pure(
            Right(Stream(
              r.Section.Types(resolved1.types),
              r.Section.Imports(resolved1.imports),
              r.Section.Functions(resolved1.funcs.map(_.tpe)),
              r.Section.Tables(resolved1.tables),
              r.Section.Memories(resolved1.mems),
              r.Section.Globals(resolved1.globals),
              r.Section.Exports(resolved1.exports)
            ) ++
              Stream.emits(resolved1.start.toSeq.map(r.Section.Start(_))) ++
              Stream(
                r.Section.Elements(resolved1.elem),
                r.Section.Code(resolved1.funcs.map {
                  case r.Func(_, locals, body) =>
                    r.FuncBody(locals.map(r.LocalEntry(1, _)), body)
                }),
                r.Section.Datas(resolved1.data)
              )
              ++ (if (debug) Stream.eval(makeNames(ctx)) else Stream.empty)))
        case (ctx, Seq(fld, fields @ _*), resolved) =>
          fld match {
            case Import(n1, n2, desc) =>
              if (ctx.importAllowed)
                desc match {
                  case ImportDesc.Func(_, TypeUse(tpe, params, results)) =>
                    for {
                      (idx, ctx1) <- resolveTypeUse(tpe, params, results, ctx, fld.pos)
                    } yield Left(
                      (ctx.copy(typedefs = ctx.typedefs ++ ctx1.typedefs).withLocalNames(Vector(), debug),
                       fields,
                       resolved.copy(imports = resolved.imports :+ r.Import
                         .Function(n1, n2, idx))))
                  case ImportDesc.Table(_, tt) =>
                    F.pure(
                      Left(
                        (ctx,
                         fields,
                         resolved.copy(imports = resolved.imports :+ r.Import
                           .Table(n1, n2, tt)))))
                  case ImportDesc.Memory(_, mt) =>
                    F.pure(
                      Left(
                        (ctx,
                         fields,
                         resolved.copy(imports = resolved.imports :+ r.Import
                           .Memory(n1, n2, mt)))))
                  case ImportDesc.Global(_, gt) =>
                    F.pure(
                      Left(
                        (ctx,
                         fields,
                         resolved.copy(imports = resolved.imports :+ r.Import
                           .Global(n1, n2, gt)))))
                }
              else
                F.raiseError(new ResolutionException("imports are not allowed after definitions", Seq(fld.pos)))
            case Type(_, _, ft) =>
              F.pure(Left((ctx, fields, resolved.copy(types = resolved.types :+ ft))))
            case Function(_, TypeUse(tpe, params, results), locals, insts) =>
              for {
                (idx, ctx1) <- resolveTypeUse(tpe, params, results, ctx, fld.pos)
                ctx2 = ctx.copy(typedefs = ctx.typedefs ++ ctx1.typedefs,
                                locals = ctx1.locals ++ locals.map(l => Def(l.id, l.pos)).toVector)
                isctx <- resolveAll(insts, ctx2)
                (insts, ctx3) = isctx
              } yield Left(
                (ctx3.copy(locals = Vector.empty).noImport.withLocalNames(ctx2.locals, debug),
                 fields,
                 resolved.copy(funcs = resolved.funcs :+ r
                   .Func(idx, locals.map(_.tpe).toVector, insts))))
            case Table(_, tt) =>
              F.pure(Left((ctx.noImport, fields, resolved.copy(tables = resolved.tables :+ tt))))
            case Memory(_, mt) =>
              F.pure(Left((ctx.noImport, fields, resolved.copy(mems = resolved.mems :+ mt))))
            case Global(_, gt, init) =>
              for {
                isctx <- resolveAll(init, ctx)
                (init, ctx) = isctx
              } yield Left((ctx.noImport, fields, resolved.copy(globals = resolved.globals :+ r.Global(gt, init))))
            case Export(n, desc) =>
              desc match {
                case ExportDesc.Func(idx) =>
                  for (idx <- resolveIndex(idx, fld.pos, ctx.funcs, "function"))
                    yield Left(
                      (ctx,
                       fields,
                       resolved.copy(exports = resolved.exports :+ r
                         .Export(n, r.ExternalKind.Function, idx))))
                case ExportDesc.Table(idx) =>
                  for (idx <- resolveIndex(idx, fld.pos, ctx.tables, "table"))
                    yield Left(
                      (ctx,
                       fields,
                       resolved.copy(exports = resolved.exports :+ r
                         .Export(n, r.ExternalKind.Table, idx))))
                case ExportDesc.Memory(idx) =>
                  for (idx <- resolveIndex(idx, fld.pos, ctx.mems, "mem"))
                    yield Left(
                      (ctx,
                       fields,
                       resolved.copy(exports = resolved.exports :+ r
                         .Export(n, r.ExternalKind.Memory, idx))))
                case ExportDesc.Global(idx) =>
                  for (idx <- resolveIndex(idx, fld.pos, ctx.globals, "global"))
                    yield Left(
                      (ctx,
                       fields,
                       resolved.copy(exports = resolved.exports :+ r
                         .Export(n, r.ExternalKind.Global, idx))))
              }
            case StartFunc(idx) =>
              resolved.start match {
                case Some(_) =>
                  F.raiseError(new ResolutionException("start function already defined", Seq(fld.pos)))
                case None =>
                  for (idx <- resolveIndex(idx, fld.pos, ctx.funcs, "function"))
                    yield Left((ctx, fields, resolved.copy(start = Some(idx))))
              }
            case Elem(tidx, offset, init) =>
              for {
                tidx <- resolveIndex(tidx, fld.pos, ctx.tables, "table")
                init <- resolveIndices(init, fld.pos, ctx.funcs, "function")
                isctx <- resolveAll(offset, ctx)
                (offset, ctx) = isctx
              } yield Left((ctx, fields, resolved.copy(elem = resolved.elem :+ r.Elem(tidx, offset, init))))
            case Data(midx, offset, data) =>
              for {
                midx <- resolveIndex(midx, fld.pos, ctx.mems, "memory")
                isctx <- resolveAll(offset, ctx)
                (offset, ctx) = isctx
              } yield Left(
                (ctx,
                 fields,
                 resolved.copy(data = resolved.data :+ r
                   .Data(midx, offset, BitVector(data)))))
          }
      }
    }
  }

  private def checkWellFormed(ctx: ResolverContext): F[Unit] =
    for {
      _ <- checkDuplicates(ctx.types, "type")
      _ <- checkDuplicates(ctx.funcs, "function")
      _ <- checkDuplicates(ctx.tables, "table")
      _ <- checkDuplicates(ctx.mems, "memory")
      _ <- checkDuplicates(ctx.globals, "global")
      _ <- checkDuplicates(ctx.locals, "local")
      _ <- checkDuplicates(ctx.labels, "label")
    } yield ()

  private def checkDuplicates(vars: Seq[Def], name: String): F[Unit] =
    F.tailRecM((vars, Map.empty[Id, Int])) {
      case (Seq(), _)                          => F.pure(Right(()))
      case (Seq(Def(NoId, _), rest @ _*), acc) => F.pure(Left((rest, acc)))
      case (Seq(Def(id, pos), rest @ _*), acc) =>
        acc.get(id) match {
          case Some(p) =>
            F.raiseError(new ResolutionException(s"$name identifier $id defined twice", Seq(pos, p)))
          case None =>
            F.pure(Left((rest, acc.updated(id, pos))))
        }
    }

  private def makeNames(ctx: ResolverContext): F[r.Section.Custom] = {
    val moduleName = ctx.name match {
      case SomeId(n) => Some(ModuleName(n))
      case _         => None
    }
    val funcNames = Some(FunctionNames(ctx.funcs.zipWithIndex.flatMap {
      case (Def(SomeId(n), _), idx) => Some(idx -> n)
      case _                        => None
    }.toMap))
    val localNames = Some(LocalNames(ctx.localNames.zipWithIndex.map {
      case (locals, fidx) =>
        (fidx, locals.zipWithIndex.flatMap {
          case (SomeId(n), idx) => Some(idx -> n)
          case _                => None
        }.toMap)
    }.toMap))
    val names = Names(Vector(moduleName, funcNames, localNames).flatten)
    NameSectionHandler.codec.encode(names) match {
      case Attempt.Successful(bv) => F.pure(r.Section.Custom("name", bv))
      case Attempt.Failure(err)   => F.raiseError(new TextCompilerException(err.messageWithContext, Seq()))
    }
  }

}
