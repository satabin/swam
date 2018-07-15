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

class SpecValidator[F[_]](implicit F: MonadError[F, Throwable]) extends Validator[F] {

  def validateValType(tpe: ValType): F[Unit] =
    Ok

  def validateResultType(tpe: ResultType): F[Unit] =
    Ok

  def validateLimits(limits: Limits): F[Unit] =
    limits match {
      case Limits(min, Some(max)) if max < min =>
        F.raiseError(new ValidationException("limits minimum must be greater or equal to minimum."))
      case _ => Ok
    }

  def validateMemType(tpe: MemType): F[Unit] =
    validateLimits(tpe.limits)

  def validateFuncType(tpe: FuncType): F[Unit] =
    if (tpe.t.size > 1)
      F.raiseError(new ValidationException("function type must have at most one return type."))
    else
      Ok

  def validateTableType(tpe: TableType): F[Unit] =
    validateLimits(tpe.limits)

  def validateElemType(tpe: ElemType): F[Unit] =
    Ok

  def validateGlobalType(tpe: GlobalType): F[Unit] =
    Ok

  def validateExternType(tpe: ExternType): F[Unit] =
    Ok

  def validate(inst: Vector[Inst], ctx: Ctx): F[Ctx] = {
    F.tailRecM((0, ctx)) {
      case (idx, ctx) =>
        if (idx >= inst.size)
          F.pure(Right(ctx))
        else
          validate(inst(idx), ctx).map(ctx => Left((idx + 1, ctx)))
    }
  }

  def validate(inst: Inst, ctx: Ctx): F[Ctx] =
    inst match {
      case Const(t) =>
        F.pure(ctx.push(t))
      case Unop(t) =>
        for {
          _ <- ctx.pop(t)
        } yield ctx
      case Binop(t) =>
        for {
          ctx <- ctx.pop(t)
          _ <- ctx.pop(t)
        } yield ctx
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
          case Some(t) => F.pure(ctx)
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
      case Load(t, offset, align) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= t.width / 8)
              for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(t)
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than ${t.width}."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case LoadN(t, n, offset, align) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= n / 8)
              for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(t)
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than $n."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
      case Store(t, offset, align) =>
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
      case StoreN(t, n, offset, align) =>
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
          case Some(_) => for (_ <- ctx.pop(ValType.I32)) yield ctx
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
              _ <- ctx.pop(tpe)
            } yield ctx
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

  def validateFunction(func: Func, ctx: Ctx): F[Unit] = {
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

  def validateTable(tpe: TableType, ctx: Ctx): F[Unit] =
    validateTableType(tpe)

  def validateMem(tpe: MemType, ctx: Ctx): F[Unit] =
    validateMemType(tpe)

  def validateGlobal(global: Global, ctx: Ctx): F[Unit] = {
    val Global(gtpe @ GlobalType(tpe, _), init) = global
    for {
      _ <- validateGlobalType(gtpe)
      _ <- validateConst(init, ctx)
      ctx <- validate(init, ctx)
      ctx <- ctx.pop(tpe)
      _ <- ctx.emptyOperands
    } yield ()
  }

  def validateElem(elem: Elem, ctx: Ctx): F[Unit] = {
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

  def validateData(data: Data, ctx: Ctx): F[Unit] = {
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

  def validateStart(start: FuncIdx, ctx: Ctx): F[Unit] =
    ctx.funcs.lift(start) match {
      case Some(FuncType(Vector(), Vector())) =>
        Ok
      case Some(_) =>
        F.raiseError(new ValidationException(s"start function cannot have parameters nor return any value."))
      case None =>
        F.raiseError(new ValidationException(s"unknown function $start."))
    }

  def validateImport(imp: Import, ctx: Ctx): F[Unit] =
    imp match {
      case Import.Function(_, _, idx) =>
        if (ctx.types.isDefinedAt(idx))
          Ok
        else
          F.raiseError(new ValidationException(s"unknown function type $idx."))
      case Import.Table(_, _, table) =>
        validateTableType(table)
      case Import.Memory(_, _, mem) =>
        validateMemType(mem)
      case Import.Global(_, _, GlobalType(_, Mut.Const)) =>
        Ok
      case Import.Global(_, _, _) =>
        F.raiseError(new ValidationException("non constant global import."))
    }

  def validateExport(exp: Export, ctx: Ctx): F[Unit] =
    exp match {
      case Export(_, ExternalKind.Function, idx) =>
        if (ctx.funcs.isDefinedAt(idx))
          Ok
        else
          F.raiseError(new ValidationException(s"unknown function $idx."))
      case Export(_, ExternalKind.Table, idx) =>
        if (ctx.tables.isDefinedAt(idx))
          Ok
        else
          F.raiseError(new ValidationException(s"unknown table $idx."))
      case Export(_, ExternalKind.Memory, idx) =>
        if (ctx.mems.isDefinedAt(idx))
          Ok
        else
          F.raiseError(new ValidationException(s"unknown memory $idx."))
      case Export(_, ExternalKind.Global, idx) =>
        ctx.globals.lift(idx) match {
          case Some(GlobalType(_, Mut.Const)) =>
            Ok
          case Some(_) =>
            F.raiseError(new ValidationException(s"non constant exported global $idx."))
          case None =>
            F.raiseError(new ValidationException(s"unknown global $idx."))
        }
    }

  def validateConst(instr: Vector[Inst], ctx: Ctx): F[Unit] =
    F.tailRecM((0, ctx)) {
      case (idx, ctx) =>
        if (idx < instr.size)
          validateConst(instr(idx), ctx).map(_ => Left((idx + 1, ctx)))
        else
          F.pure(Right(()))
    }

  def validateConst(instr: Inst, ctx: Ctx): F[Unit] =
    instr match {
      case Const(_) => Ok
      case GetGlobal(x) =>
        ctx.globals.lift(x) match {
          case Some(GlobalType(_, Mut.Const)) =>
            Ok
          case Some(_) =>
            F.raiseError(new ValidationException(s"non constant global $x."))
          case None =>
            F.raiseError(new ValidationException(s"unknown global $x."))
        }
      case _ =>
        F.raiseError(new ValidationException(s"non constant instruction $instr."))
    }

}
