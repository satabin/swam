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
package validation

import syntax._

import cats._
import cats.implicits._

import scala.language.higherKinds

import fs2._

class SpecValidator[F[_]](dataHardMax: Int) extends Validator[F] {

  type Ctx = Context[F]

  def validateAll[T](elements: Vector[T], validation: T => F[Unit])(implicit F: MonadError[F, Throwable]): F[Unit] =
    F.tailRecM(0) { idx =>
      if (idx < elements.size)
        validation(elements(idx)).map(_ => Left(idx + 1))
      else
        F.pure(Right(()))
    }

  def validateAll[T](elements: Vector[T], ctx: Ctx, validation: (T, Ctx) => F[Unit])(
      implicit F: MonadError[F, Throwable]): F[Unit] =
    F.tailRecM(0) { idx =>
      if (idx < elements.size)
        validation(elements(idx), ctx).map(_ => Left(idx + 1))
      else
        F.pure(Right(()))
    }

  def validateValType(tpe: ValType)(implicit F: MonadError[F, Throwable]): F[Unit] =
    F.pure(())

  def validateResultType(tpe: ResultType)(implicit F: MonadError[F, Throwable]): F[Unit] =
    F.pure(())

  def validateLimits(limits: Limits)(implicit F: MonadError[F, Throwable]): F[Unit] =
    limits match {
      case Limits(min, Some(max)) if max < min =>
        F.raiseError(new ValidationException("limits minimum must be greater or equal to minimum."))
      case _ => F.pure(())
    }

  def validateMemType(tpe: MemType)(implicit F: MonadError[F, Throwable]): F[Unit] =
    for {
      _ <- validateLimits(tpe.limits)
      _ <- validateHardMax(tpe.limits.min)
      _ <- validateHardMax(tpe.limits.max)
    } yield ()

  def validateHardMax(i: Option[Int])(implicit F: MonadError[F, Throwable]): F[Unit] =
    i match {
      case Some(i) => validateHardMax(i)
      case None    => F.pure(())
    }

  def validateHardMax(i: Int)(implicit F: MonadError[F, Throwable]): F[Unit] =
    if (i < 0 || i > dataHardMax)
      F.raiseError(new ValidationException(s"memory size may not exceed $dataHardMax pages"))
    else
      F.pure(())

  def validateFuncType(tpe: FuncType)(implicit F: MonadError[F, Throwable]): F[Unit] =
    if (tpe.t.size > 1)
      F.raiseError(new ValidationException("function type must have at most one return type."))
    else
      F.pure(())

  def validateTableType(tpe: TableType)(implicit F: MonadError[F, Throwable]): F[Unit] =
    validateLimits(tpe.limits)

  def validateElemType(tpe: ElemType)(implicit F: MonadError[F, Throwable]): F[Unit] =
    F.pure(())

  def validateGlobalType(tpe: GlobalType)(implicit F: MonadError[F, Throwable]): F[Unit] =
    F.pure(())

  def validate(inst: Vector[Inst], ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Ctx] = {
    F.tailRecM((0, ctx)) {
      case (idx, ctx) =>
        if (idx >= inst.size)
          F.pure(Right(ctx))
        else
          validate(inst(idx), ctx).map(ctx => Left((idx + 1, ctx)))
    }
  }

  def validate(inst: Inst, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Ctx] =
    inst match {
      case Const(t) =>
        F.pure(ctx.push(t))
      case Unop(t) =>
        for {
          ctx <- ctx.pop(t)
        } yield ctx.push(t)
      case Binop(t) =>
        for {
          ctx <- ctx.pop(t)
          ctx <- ctx.pop(t)
        } yield ctx.push(t)
      case Testop(t) =>
        for {
          ctx <- ctx.pop(t)
        } yield ctx.push(ValType.I32)
      case Relop(t) =>
        for {
          ctx <- ctx.pop(t)
          ctx <- ctx.pop(t)
        } yield ctx.push(ValType.I32)
      case Convertop(from, to) =>
        for {
          ctx <- ctx.pop(from)
        } yield ctx.push(to)
      case Drop =>
        for {
          ctx <- ctx.pop
        } yield ctx._2
      case Select =>
        for {
          ctx <- ctx.pop(ValType.I32)
          ctx <- ctx.pop
          (t, ctxb) = ctx
          ctx <- ctxb.pop(t)
        } yield ctx.push(t)
      case GetLocal(x) =>
        ctx.locals.lift(x) match {
          case Some(t) => F.pure(ctx.push(t))
          case None =>
            F.raiseError(new ValidationException(s"unknown local $x."))
        }
      case SetLocal(x) =>
        ctx.locals.lift(x) match {
          case Some(t) => for (ctx <- ctx.pop(t)) yield ctx
          case None =>
            F.raiseError(new ValidationException(s"unknown local $x."))
        }
      case TeeLocal(x) =>
        ctx.locals.lift(x) match {
          case Some(t) =>
            for (ctx <- ctx.pop(t)) yield ctx.push(t)
          case None =>
            F.raiseError(new ValidationException(s"unknown local $x."))
        }
      case GetGlobal(x) =>
        ctx.globals.lift(x) match {
          case Some(GlobalType(t, _)) => F.pure(ctx.push(t))
          case None =>
            F.raiseError(new ValidationException(s"unknown global $x."))
        }
      case SetGlobal(x) =>
        ctx.globals.lift(x) match {
          case Some(GlobalType(t, Mut.Var)) => for (ctx <- ctx.pop(t)) yield ctx
          case Some(_) =>
            F.raiseError(new ValidationException(s"global variable $x is not mutable"))
          case None =>
            F.raiseError(new ValidationException(s"unknown global $x."))
        }
      case Load(t, align, offset) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= t.width / 8)
              for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(t)
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than ${t.width}."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case LoadN(t, n, align, offset) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= n / 8)
              for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(t)
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than $n."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case Store(t, align, offset) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= t.width / 8)
              for {
                ctx <- ctx.pop(t)
                ctx <- ctx.pop(ValType.I32)
              } yield ctx
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than ${t.width}."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case StoreN(t, n, align, offset) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= n / 8)
              for {
                ctx <- ctx.pop(t)
                ctx <- ctx.pop(ValType.I32)
              } yield ctx
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than $n."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case MemorySize =>
        ctx.mems.lift(0) match {
          case Some(_) => F.pure(ctx.push(ValType.I32))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case MemoryGrow =>
        ctx.mems.lift(0) match {
          case Some(_) => for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(ValType.I32)
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case Nop =>
        F.pure(ctx)
      case Unreachable =>
        F.pure(ctx.markUnreachable)
      case Block(rt @ ResultType(Some(tpe)), instr) =>
        for {
          ctxb <- validate(instr, ctx.withLabels(Vector(rt)).withFreshOperands)
          ctxb <- ctxb.pop(tpe)
          _ <- ctxb.emptyOperands
        } yield ctx.push(tpe)
      case Block(rt @ ResultType(None), instr) =>
        for {
          ctxb <- validate(instr, ctx.withLabels(Vector(rt)).withFreshOperands)
          _ <- ctxb.emptyOperands
        } yield ctx
      case Loop(rt @ ResultType(Some(tpe)), instr) =>
        for {
          ctxb <- validate(instr, ctx.withLabels(Vector(ResultType(None))).withFreshOperands)
          ctxb <- ctxb.pop(tpe)
          _ <- ctxb.emptyOperands
        } yield ctx.push(tpe)
      case Loop(rt @ ResultType(None), instr) =>
        for {
          ctxb <- validate(instr, ctx.withLabels(Vector(ResultType(None))).withFreshOperands)
          _ <- ctxb.emptyOperands
        } yield ctx
      case If(rt @ ResultType(Some(tpe)), thenInst, elseInst) =>
        for {
          ctx <- ctx.pop(ValType.I32)
          fresh = ctx.withLabels(Vector(rt)).withFreshOperands
          ctxt <- validate(thenInst, fresh)
          ctxt <- ctxt.pop(tpe)
          _ <- ctxt.emptyOperands
          ctxe <- validate(elseInst, fresh)
          ctxe <- ctxe.pop(tpe)
          _ <- ctxe.emptyOperands
        } yield ctx.push(tpe)
      case If(rt @ ResultType(None), thenInst, elseInst) =>
        for {
          ctx <- ctx.pop(ValType.I32)
          fresh = ctx.withLabels(Vector(rt)).withFreshOperands
          ctxt <- validate(thenInst, fresh)
          _ <- ctxt.emptyOperands
          ctxe <- validate(elseInst, fresh)
          _ <- ctxe.emptyOperands
        } yield ctx
      case Br(l) =>
        ctx.labels.lift(l) match {
          case Some(ResultType(Some(tpe))) =>
            for (ctx <- ctx.pop(tpe)) yield ctx.markUnreachable
          case Some(ResultType(None)) => F.pure(ctx.markUnreachable)
          case None =>
            F.raiseError(new ValidationException(s"unknown label $l."))
        }
      case BrIf(l) =>
        ctx.labels.lift(l) match {
          case Some(ResultType(Some(tpe))) =>
            for {
              ctx <- ctx.pop(ValType.I32)
              ctx <- ctx.pop(tpe)
            } yield ctx.push(tpe)
          case Some(ResultType(None)) =>
            for {
              ctx <- ctx.pop(ValType.I32)
            } yield ctx
          case None =>
            F.raiseError(new ValidationException(s"unknown label $l."))
        }
      case BrTable(table, lbl) =>
        ctx.labels.lift(lbl) match {
          case rt @ Some(ResultType(Some(tpe))) =>
            if (table.forall(ctx.labels.lift(_) == rt))
              for {
                ctx <- ctx.pop(ValType.I32)
                ctx <- ctx.pop(tpe)
              } yield ctx.markUnreachable
            else
              F.raiseError(new ValidationException(s"all table labels must be of the same return type $tpe."))
          case rt @ Some(ResultType(None)) =>
            if (table.forall(ctx.labels.lift(_) == rt))
              for {
                ctx <- ctx.pop(ValType.I32)
              } yield ctx.markUnreachable
            else
              F.raiseError(new ValidationException(s"all table labels must be of empty return type."))
          case None =>
            F.raiseError(new ValidationException(s"unknown label $lbl."))
        }
      case Return =>
        ctx.ret match {
          case Some(ResultType(Some(tpe))) =>
            for (ctx <- ctx.pop(tpe)) yield ctx.markUnreachable
          case Some(ResultType(None)) =>
            F.pure(ctx.markUnreachable)
          case None =>
            F.raiseError(new ValidationException("return must occur in a function context."))
        }
      case Call(x) =>
        ctx.funcs.lift(x) match {
          case Some(FuncType(ins, outs)) =>
            for (ctx <- ctx.pop(ins)) yield ctx.push(outs)
          case None =>
            F.raiseError(new ValidationException(s"unknown function $x."))
        }
      case CallIndirect(x) =>
        ctx.tables.lift(0) match {
          case Some(TableType(ElemType.AnyFunc, _)) =>
            ctx.types.lift(x) match {
              case Some(FuncType(ins, outs)) =>
                for {
                  ctx <- ctx.pop(ValType.I32)
                  ctx <- ctx.pop(ins)
                } yield ctx.push(outs)
              case None =>
                F.raiseError(new ValidationException(s"unknown type $x."))
            }
          case None =>
            F.raiseError(new ValidationException("unknown table 0."))
        }
      case _ =>
        F.raiseError(new ValidationException(s"unknown instruction $inst."))
    }

  def validateFunction(func: Func, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] = {
    val Func(tpe, locals, body) = func
    ctx.types.lift(tpe) match {
      case Some(FuncType(ins, out)) =>
        out.headOption match {
          case ret @ Some(tpe) =>
            for {
              ctxb <- validate(body,
                               ctx.copy[F](locals = ins ++ locals,
                                           labels = Vector(ResultType(ret)),
                                           ret = Some(ResultType(ret)),
                                           operands = Nil))
              ctxb <- ctxb.pop(tpe)
              _ <- ctxb.emptyOperands
            } yield ()
          case None =>
            for {
              ctxb <- validate(body,
                               ctx.copy[F](locals = ins ++ locals,
                                           labels = Vector(ResultType(None)),
                                           ret = Some(ResultType(None)),
                                           operands = Nil))
              _ <- ctxb.emptyOperands
            } yield ()
        }
      case None =>
        F.raiseError(new ValidationException(s"unknown type $tpe."))
    }
  }

  def validateTable(tpe: TableType, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] =
    validateTableType(tpe)

  def validateMem(tpe: MemType, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] =
    validateMemType(tpe)

  def validateGlobal(global: Global, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] = {
    val Global(gtpe @ GlobalType(tpe, _), init) = global
    for {
      _ <- validateGlobalType(gtpe)
      _ <- validateConst(init, ctx)
      ctx <- validate(init, ctx)
      ctx <- ctx.pop(tpe)
      _ <- ctx.emptyOperands
    } yield ()
  }

  def validateElem(elem: Elem, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] = {
    val Elem(table, offset, init) = elem
    ctx.tables.lift(table) match {
      case Some(_) =>
        init.find(!ctx.funcs.isDefinedAt(_)) match {
          case Some(y) =>
            F.raiseError(new ValidationException(s"unknown init functions $y."))
          case None =>
            for {
              _ <- validateConst(offset, ctx)
              ctx1 <- validate(offset, ctx)
              ctx1 <- ctx1.pop(ValType.I32)
              _ <- ctx1.emptyOperands
            } yield ()
        }
      case None =>
        F.raiseError(new ValidationException(s"unknown table $table."))
    }
  }

  def validateData(data: Data, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] = {
    val Data(d, offset, init) = data
    ctx.mems.lift(d) match {
      case Some(_) =>
        for {
          _ <- validateConst(offset, ctx)
          ctx1 <- validate(offset, ctx)
          ctx1 <- ctx1.pop(ValType.I32)
          _ <- ctx1.emptyOperands
        } yield ()
      case None =>
        F.raiseError(new ValidationException(s"unknown mem $d."))
    }
  }

  def validateStart(start: FuncIdx, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] =
    ctx.funcs.lift(start) match {
      case Some(FuncType(Vector(), Vector())) =>
        F.pure(())
      case Some(_) =>
        F.raiseError(new ValidationException(s"start function cannot have parameters nor return any value."))
      case None =>
        F.raiseError(new ValidationException(s"unknown function $start."))
    }

  def validateImport(imp: Import, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] =
    imp match {
      case Import.Function(_, _, idx) =>
        if (ctx.types.isDefinedAt(idx))
          F.pure(())
        else
          F.raiseError(new ValidationException(s"unknown function type $idx."))
      case Import.Table(_, _, table) =>
        validateTableType(table)
      case Import.Memory(_, _, mem) =>
        validateMemType(mem)
      case Import.Global(_, _, _) =>
        F.pure(())
    }

  def validateExport(exp: Export, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] =
    exp match {
      case Export(_, ExternalKind.Function, idx) =>
        if (ctx.funcs.isDefinedAt(idx))
          F.pure(())
        else
          F.raiseError(new ValidationException(s"unknown function $idx."))
      case Export(_, ExternalKind.Table, idx) =>
        if (ctx.tables.isDefinedAt(idx))
          F.pure(())
        else
          F.raiseError(new ValidationException(s"unknown table $idx."))
      case Export(_, ExternalKind.Memory, idx) =>
        if (ctx.mems.isDefinedAt(idx))
          F.pure(())
        else
          F.raiseError(new ValidationException(s"unknown memory $idx."))
      case Export(_, ExternalKind.Global, idx) =>
        ctx.globals.lift(idx) match {
          case Some(_) =>
            F.pure(())
          case None =>
            F.raiseError(new ValidationException(s"unknown global $idx."))
        }
    }

  def validateConst(instr: Vector[Inst], ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] =
    F.tailRecM((0, ctx)) {
      case (idx, ctx) =>
        if (idx < instr.size)
          validateConst(instr(idx), ctx).map(_ => Left((idx + 1, ctx)))
        else
          F.pure(Right(()))
    }

  def validateConst(instr: Inst, ctx: Ctx)(implicit F: MonadError[F, Throwable]): F[Unit] =
    instr match {
      case Const(_) => F.pure(())
      case GetGlobal(x) =>
        ctx.globals.lift(x) match {
          case Some(_) =>
            F.pure(())
          case None =>
            F.raiseError(new ValidationException(s"unknown global $x."))
        }
      case _ =>
        F.raiseError(new ValidationException(s"non constant instruction $instr."))
    }

  private object imported {

    def funcs(types: Vector[FuncType], imports: Vector[Import]): Vector[FuncType] = imports.collect {
      case Import.Function(_, _, idx) => types(idx)
    }

    def tables(imports: Vector[Import]): Vector[TableType] = imports.collect {
      case Import.Table(_, _, tpe) => tpe
    }

    def mems(imports: Vector[Import]): Vector[MemType] = imports.collect {
      case Import.Memory(_, _, tpe) => tpe
    }

    def globals(imports: Vector[Import]): Vector[GlobalType] = imports.collect {
      case Import.Global(_, _, tpe) => tpe
    }

  }

  private type Acc = ((Int, Vector[Int], Vector[Import], Context[F]), Section)

  def validate(stream: Stream[F, Section])(implicit F: MonadError[F, Throwable]): Stream[F, Section] =
    stream
      .evalMapAccumulate((0, Vector.empty[Int], Vector.empty[Import], EmptyContext[F])) { (acc, sec) =>
        acc match {
          case (idx, tpes, imports, ctx) =>
            if (sec.id > 0 && sec.id <= idx)
              F.raiseError[Acc](
                new ValidationException(s"${nameOf(sec.id)} section may not appear after ${nameOf(idx)} section")
              )
            else
              sec match {
                case Section.Types(functypes) =>
                  for (_ <- validateAll(functypes, validateFuncType))
                    yield ((sec.id, tpes, imports, ctx.copy[F](types = functypes)), sec)
                case Section.Imports(imports) =>
                  for (_ <- validateAll(imports, ctx, validateImport))
                    yield {
                      val ctx1 = ctx.copy[F](funcs = imported.funcs(ctx.types, imports),
                                             tables = imported.tables(imports),
                                             mems = imported.mems(imports),
                                             globals = imported.globals(imports))
                      ((sec.id, tpes, imports, ctx1), sec)
                    }
                case Section.Functions(tpes) =>
                  F.pure {
                    val funcs = tpes.map(ctx.types(_))
                    ((sec.id, tpes, imports, ctx.copy[F](funcs = ctx.funcs ++ funcs)), sec)
                  }
                case Section.Tables(tables) =>
                  if (ctx.tables.size + tables.size > 1)
                    F.raiseError[Acc](new ValidationException("at most one table is allowed."))
                  else
                    for (_ <- validateAll(tables, validateTableType))
                      yield ((sec.id, tpes, imports, ctx.copy[F](tables = ctx.tables ++ tables)), sec)
                case Section.Memories(mems) =>
                  if (ctx.mems.size + mems.size > 1)
                    F.raiseError[Acc](new ValidationException("at most one memory is allowed."))
                  else
                    for (_ <- validateAll(mems, validateMemType))
                      yield ((sec.id, tpes, imports, ctx.copy[F](mems = ctx.mems ++ mems)), sec)
                case Section.Globals(globals) =>
                  for (_ <- validateAll(globals,
                                        EmptyContext[F].copy[F](globals = imported.globals(imports)),
                                        validateGlobal))
                    yield ((sec.id, tpes, imports, ctx.copy[F](globals = ctx.globals ++ globals.map(_.tpe))), sec)
                case Section.Exports(exports) =>
                  val duplicate = exports
                    .groupBy(_.fieldName)
                    .mapValues(_.size)
                    .find(_._2 > 1)
                  duplicate match {
                    case Some((name, _)) =>
                      F.raiseError[Acc](new ValidationException(s"duplicate export name $name."))
                    case None =>
                      for (_ <- validateAll(exports, ctx, validateExport))
                        yield ((sec.id, tpes, imports, ctx), sec)
                  }
                case Section.Start(start) =>
                  for (_ <- validateStart(start, ctx))
                    yield ((sec.id, tpes, imports, ctx), sec)
                case Section.Elements(elem) =>
                  for (_ <- validateAll(elem, ctx, validateElem))
                    yield ((sec.id, tpes, imports, ctx), sec)
                case Section.Code(code) =>
                  if (code.size != tpes.size)
                    F.raiseError[Acc](
                      new ValidationException("code and function sections must have the same number of elements")
                    )
                  else {
                    val funcs = code.zip(tpes).map {
                      case (FuncBody(locals, code), typeIdx) =>
                        val locs = locals.flatMap {
                          case LocalEntry(count, tpe) =>
                            Vector.fill(count)(tpe)
                        }
                        Func(typeIdx, locs, code)
                    }
                    for (_ <- validateAll(funcs, ctx, validateFunction))
                      yield ((sec.id, tpes, imports, ctx), sec)
                  }
                case Section.Datas(data) =>
                  for (_ <- validateAll(data, ctx, validateData))
                    yield ((sec.id, tpes, imports, ctx), sec)
                case Section.Custom(_, _) =>
                  // ignore the custom sections
                  F.pure((acc, sec))
              }
        }
      }
      .map(_._2)

  private def nameOf(id: Int) =
    id match {
      case 0  => "custom"
      case 1  => "type"
      case 2  => "import"
      case 3  => "function"
      case 4  => "table"
      case 5  => "memory"
      case 6  => "global"
      case 7  => "export"
      case 8  => "start"
      case 9  => "element"
      case 10 => "code"
      case 11 => "data"
      case _  => "unknown"
    }

}
