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
import traversal._

import cats._
import cats.implicits._

import fs2._

private class SpecValidator[F[_]](dataHardMax: Int)(implicit F: MonadError[F, Throwable]) extends Validator[F] {

  private type Ctx = Context[F]

  private def validateAll[T](elements: Vector[T], validation: T => F[Unit]): F[Unit] =
    F.tailRecM(0) { idx =>
      if (idx < elements.size)
        validation(elements(idx)).map(_ => Left(idx + 1))
      else
        F.pure(Right(()))
    }

  private def validateAll[T](elements: Vector[T], ctx: Ctx, validation: (T, Ctx) => F[Unit]): F[Unit] =
    F.tailRecM(0) { idx =>
      if (idx < elements.size)
        validation(elements(idx), ctx).map(_ => Left(idx + 1))
      else
        F.pure(Right(()))
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

  private type Acc = (Int, Vector[Int], Vector[Import], Int, Context[F])

  private object ctraverser extends Traverser[F, Ctx] {
    override val constTraverse = {
      case (ctx, AConst(t)) => F.pure(ctx.push(t))
    }

    override val unopTraverse = {
      case (ctx, Unop(t)) =>
        for {
          ctx <- ctx.pop(t)
        } yield ctx.push(t)
    }

    override val binopTraverse = {
      case (ctx, Binop(t)) =>
        for {
          ctx <- ctx.pop(t)
          ctx <- ctx.pop(t)
        } yield ctx.push(t)
    }

    override val testopTraverse = {
      case (ctx, Testop(t)) =>
        for {
          ctx <- ctx.pop(t)
        } yield ctx.push(ValType.I32)
    }

    override val relopTraverse = {
      case (ctx, Relop(t)) =>
        for {
          ctx <- ctx.pop(t)
          ctx <- ctx.pop(t)
        } yield ctx.push(ValType.I32)
    }

    override val convertopTraverse = {
      case (ctx, Convertop(from, to)) =>
        for {
          ctx <- ctx.pop(from)
        } yield ctx.push(to)
    }

    override val satConvertopTraverse = {
      case (ctx, SatConvertop(from, to)) =>
        for {
          ctx <- ctx.pop(from)
        } yield ctx.push(to)
    }

    override val dropTraverse = {
      case (ctx, Drop) =>
        for {
          ctx <- ctx.pop
        } yield ctx._2
    }

    override val selectTraverse = {
      case (ctx, Select) =>
        for {
          ctx <- ctx.pop(ValType.I32)
          ctx <- ctx.pop
          (t, ctxb) = ctx
          ctx <- ctxb.pop(t)
        } yield ctx.push(t)
    }

    override val localGetTraverse = {
      case (ctx, LocalGet(x)) =>
        ctx.locals.lift(x) match {
          case Some(t) => F.pure(ctx.push(t))
          case None =>
            F.raiseError(new ValidationException(s"unknown local $x."))
        }
    }

    override val localSetTraverse = {
      case (ctx, LocalSet(x)) =>
        ctx.locals.lift(x) match {
          case Some(t) => for (ctx <- ctx.pop(t)) yield ctx
          case None =>
            F.raiseError(new ValidationException(s"unknown local $x."))
        }
    }

    override val localTeeTraverse = {
      case (ctx, LocalTee(x)) =>
        ctx.locals.lift(x) match {
          case Some(t) =>
            for (ctx <- ctx.pop(t)) yield ctx.push(t)
          case None =>
            F.raiseError(new ValidationException(s"unknown local $x."))
        }
    }

    override val globalGetTraverse = {
      case (ctx, GlobalGet(x)) =>
        ctx.globals.lift(x) match {
          case Some(GlobalType(t, _)) => F.pure(ctx.push(t))
          case None =>
            F.raiseError(new ValidationException(s"unknown global $x."))
        }
    }

    override val globalSetTraverse = {
      case (ctx, GlobalSet(x)) =>
        ctx.globals.lift(x) match {
          case Some(GlobalType(t, Mut.Var)) => for (ctx <- ctx.pop(t)) yield ctx
          case Some(_) =>
            F.raiseError(new ValidationException(s"global variable $x is not mutable"))
          case None =>
            F.raiseError(new ValidationException(s"unknown global $x."))
        }
    }

    override val loadInstTraverse = {
      case (ctx, Load(t, align, offset)) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= t.width / 8)
              for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(t)
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than ${t.width}."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
    }

    override val loadNInstTraverse = {
      case (ctx, LoadN(t, n, align, offset)) =>
        ctx.mems.lift(0) match {
          case Some(_) =>
            if ((1 << align) <= n / 8)
              for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(t)
            else
              F.raiseError(new ValidationException(s"alignment must not be larger than $n."))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
    }

    override val storeInstTraverse = {
      case (ctx, Store(t, align, offset)) =>
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
    }

    override val storeNInstTraverse = {
      case (ctx, StoreN(t, n, align, offset)) =>
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
    }

    override val memorySizeTraverse = {
      case (ctx, MemorySize) =>
        ctx.mems.lift(0) match {
          case Some(_) => F.pure(ctx.push(ValType.I32))
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
    }

    override val memoryGrowTraverse = {
      case (ctx, MemoryGrow) =>
        ctx.mems.lift(0) match {
          case Some(_) => for (ctx <- ctx.pop(ValType.I32)) yield ctx.push(ValType.I32)
          case None =>
            F.raiseError(new ValidationException("unknown memory 0."))
        }
    }

    override val unreachableTraverse = {
      case (ctx, Unreachable) =>
        F.pure(ctx.markUnreachable)
    }

    override val blockPrepare = {
      case (ctx, Block(bt, _)) =>
        for {
          params <- bt.params(ctx.types).liftTo[F](new ValidationException(s"unknown block type $bt"))
          _ <- ctx.pop(params)
          ctx <- ctx.enterContext(bt, false)
        } yield ctx
    }

    override val blockTraverse = {
      case (ctx, Block(bt @ BlockType.ValueType(tpe), instr)) =>
        for {
          ctx <- ctx.pop(tpe)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx.push(tpe)
      case (ctx, Block(bt @ BlockType.NoType, instr)) =>
        for {
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx
      case (ctx, Block(bt @ BlockType.FunctionType(fidx), instr)) =>
        val tpes = ctx.types(fidx).t
        for {
          ctx <- ctx.pop(tpes)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx.push(tpes)
    }

    override val loopPrepare = {
      case (ctx, Loop(bt, _)) =>
        for {
          params <- bt.params(ctx.types).liftTo[F](new ValidationException(s"unknown loop type $bt"))
          _ <- ctx.pop(params)
          ctx <- ctx.enterContext(bt, true)
        } yield ctx
    }

    override val loopTraverse = {
      case (ctx, Loop(bt @ BlockType.ValueType(tpe), instr)) =>
        for {
          ctx <- ctx.pop(tpe)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx.push(tpe)
      case (ctx, Loop(bt @ BlockType.NoType, instr)) =>
        for {
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx
      case (ctx, Loop(bt @ BlockType.FunctionType(fidx), instr)) =>
        val tpes = ctx.types(fidx).t
        for {
          ctx <- ctx.pop(tpes)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx.push(tpes)
    }

    override val thenPrepare = {
      case (ctx, If(bt, _, _)) =>
        for {
          ctx <- ctx.pop(ValType.I32)
          params <- bt.params(ctx.types).liftTo[F](new ValidationException(s"unknown if type $bt"))
          _ <- ctx.pop(params)
          ctx <- ctx.enterContext(bt, false)
        } yield ctx
    }

    // validate and leave the then context
    override val elsePrepare = {
      case (ctx, If(bt @ BlockType.ValueType(tpe), _, _)) =>
        for {
          ctx <- ctx.pop(tpe)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, false)
          // no need to revalidate the parameter types
          ctx <- ctx.enterContext(bt, false)
        } yield ctx
      case (ctx, If(bt @ BlockType.NoType, thenInst, elseInst)) =>
        for {
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, false)
          // no need to revalidate the parameter types
          ctx <- ctx.enterContext(bt, false)
        } yield ctx
      case (ctx, If(bt @ BlockType.FunctionType(fidx), thenInst, elseInst)) =>
        val tpes = ctx.types(fidx).t
        for {
          ctx <- ctx.pop(tpes)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, false)
          // no need to revalidate the parameter types
          ctx <- ctx.enterContext(bt, false)
        } yield ctx
    }

    // validate and leave the else context
    override val ifTraverse = {
      case (ctx, If(bt @ BlockType.ValueType(tpe), thenInst, elseInst)) =>
        for {
          ctx <- ctx.pop(tpe)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx.push(tpe)
      case (ctx, If(bt @ BlockType.NoType, thenInst, elseInst)) =>
        for {
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx
      case (ctx, If(bt @ BlockType.FunctionType(fidx), thenInst, elseInst)) =>
        val tpes = ctx.types(fidx).t
        for {
          ctx <- ctx.pop(tpes)
          _ <- ctx.emptyOperands
          ctx <- ctx.leaveContext(bt, true)
        } yield ctx.push(tpes)
    }

    override val brTraverse = {
      case (ctx, Br(l)) =>
        ctx.labels.lift(l) match {
          case Some(ResultType(tpes)) =>
            for (ctx <- ctx.pop(tpes)) yield ctx.markUnreachable
          case None =>
            F.raiseError(new ValidationException(s"unknown label $l."))
        }
    }

    override val brIfTraverse = {
      case (ctx, BrIf(l)) =>
        ctx.labels.lift(l) match {
          case Some(ResultType(tpes)) =>
            for {
              ctx <- ctx.pop(ValType.I32)
              ctx <- ctx.pop(tpes)
            } yield ctx.push(tpes)
          case None =>
            F.raiseError(new ValidationException(s"unknown label $l."))
        }
    }

    override val brTableTraverse = {
      case (ctx, BrTable(table, lbl)) =>
        ctx.labels.lift(lbl) match {
          case rt @ Some(ResultType(tpes)) =>
            if (table.forall(ctx.labels.lift(_) == rt))
              for {
                ctx <- ctx.pop(ValType.I32)
                ctx <- ctx.pop(tpes)
              } yield ctx.markUnreachable
            else
              F.raiseError(
                new ValidationException(
                  s"all table labels must be of the same return types ${tpes.mkString("[", ", ", "]")}."))
          case None =>
            F.raiseError(new ValidationException(s"unknown label $lbl."))
        }
    }

    override val returnTraverse = {
      case (ctx, Return) =>
        ctx.ret match {
          case Some(ResultType(tpes)) =>
            for (ctx <- ctx.pop(tpes)) yield ctx.markUnreachable
          case None =>
            F.raiseError(new ValidationException("return must occur in a function context."))
        }
    }

    override val callTraverse = {
      case (ctx, Call(x)) =>
        ctx.funcs.lift(x) match {
          case Some(FuncType(ins, outs)) =>
            for (ctx <- ctx.pop(ins)) yield ctx.push(outs)
          case None =>
            F.raiseError(new ValidationException(s"unknown function $x."))
        }
    }

    override val callIndirectTraverse = {
      case (ctx, CallIndirect(x)) =>
        ctx.tables.lift(0) match {
          case Some(TableType(ElemType.FuncRef, _)) =>
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
    }
  }

  private object straverser extends SectionTraverser[F, Acc] {

    // custom sections are not validated, so no override here for them

    def validateFuncType(tpe: FuncType): F[Unit] =
      F.pure(())

    def validateData(data: Data, ctx: Ctx): F[Unit] = {
      val Data(d, offset, init) = data
      ctx.mems.lift(d) match {
        case Some(_) =>
          for {
            _ <- validateConst(offset, ctx)
            ctx1 <- validateExpr(ctx, offset)
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
          F.pure(())
        case Some(_) =>
          F.raiseError(new ValidationException(s"start function cannot have parameters nor return any value."))
        case None =>
          F.raiseError(new ValidationException(s"unknown function $start."))
      }

    def validateImport(imp: Import, ctx: Ctx): F[Unit] =
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

    def validateExport(exp: Export, ctx: Ctx): F[Unit] =
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

    def validateLimits(limits: Limits): F[Unit] =
      limits match {
        case Limits(min, Some(max)) if max < min =>
          F.raiseError(new ValidationException(s"limits maximum must be greater or equal to minimum ($max > $min)."))
        case _ => F.pure(())
      }

    def validateMemType(tpe: MemType): F[Unit] =
      for {
        _ <- validateLimits(tpe.limits)
        _ <- validateHardMax(tpe.limits.min)
        _ <- validateHardMax(tpe.limits.max)
      } yield ()

    def validateHardMax(i: Option[Int]): F[Unit] =
      i match {
        case Some(i) => validateHardMax(i)
        case None    => F.pure(())
      }

    def validateHardMax(i: Int): F[Unit] =
      if (i < 0 || i > dataHardMax)
        F.raiseError(new ValidationException(s"memory size may not exceed $dataHardMax pages"))
      else
        F.pure(())

    def validateTableType(tpe: TableType): F[Unit] =
      validateLimits(tpe.limits)

    def validateElemType(tpe: ElemType): F[Unit] =
      F.pure(())

    def validateExpr(ctx: Ctx, expr: Expr): F[Ctx] =
      expr.foldM(ctx)(ctraverser.run(_, _))

    def validateFunction(func: Func, ctx: Ctx): F[Unit] = {
      val Func(tpe, locals, body) = func
      ctx.types.lift(tpe) match {
        case Some(FuncType(ins, out)) =>
          for {
            ctxb <- validateExpr(ctx.copy[F](locals = ins ++ locals,
                                             labels = Vector(ResultType(out)),
                                             ret = Some(ResultType(out)),
                                             operands = Nil),
                                 body)
            ctxb <- ctxb.pop(out)
            _ <- ctxb.emptyOperands
          } yield ()
        case None =>
          F.raiseError(new ValidationException(s"unknown type $tpe."))
      }
    }

    def validateGlobalType(tpe: GlobalType): F[Unit] =
      F.pure(())

    def validateGlobal(global: Global, ctx: Ctx): F[Unit] = {
      val Global(gtpe @ GlobalType(tpe, _), init) = global
      for {
        _ <- validateGlobalType(gtpe)
        _ <- validateConst(init, ctx)
        ctx <- validateExpr(ctx, init)
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
                ctx1 <- validateExpr(ctx, offset)
                ctx1 <- ctx1.pop(ValType.I32)
                _ <- ctx1.emptyOperands
              } yield ()
          }
        case None =>
          F.raiseError(new ValidationException(s"unknown table $table."))
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
        case AConst(_) => F.pure(())
        case GlobalGet(x) =>
          ctx.globals.lift(x) match {
            case Some(_) =>
              F.pure(())
            case None =>
              F.raiseError(new ValidationException(s"unknown global $x."))
          }
        case _ =>
          F.raiseError(new ValidationException(s"non constant instruction $instr."))
      }

    override val typesTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Types(functypes)) =>
        for (_ <- validateAll(functypes, validateFuncType))
          yield (sec.id, tpes, imports, codes, ctx.copy[F](types = functypes))
    }

    override val importsTraverse = {
      case ((idx, tpes, _, codes, ctx), sec @ Section.Imports(imports)) =>
        for (_ <- validateAll(imports, ctx, validateImport))
          yield {
            val ctx1 = ctx.copy[F](funcs = imported.funcs(ctx.types, imports),
                                   tables = imported.tables(imports),
                                   mems = imported.mems(imports),
                                   globals = imported.globals(imports))
            (sec.id, tpes, imports, codes, ctx1)
          }
    }

    override val functionsTraverse = {
      case ((idx, _, imports, codes, ctx), sec @ Section.Functions(tpes)) =>
        F.pure {
          val funcs = tpes.map(ctx.types(_))
          (sec.id, tpes, imports, codes, ctx.copy[F](funcs = ctx.funcs ++ funcs))
        }
    }

    override val tablesTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Tables(tables)) =>
        if (ctx.tables.size + tables.size > 1)
          F.raiseError[Acc](new ValidationException("at most one table is allowed."))
        else
          for (_ <- validateAll(tables, validateTableType))
            yield (sec.id, tpes, imports, codes, ctx.copy[F](tables = ctx.tables ++ tables))
        if (ctx.tables.size + tables.size > 1)
          F.raiseError[Acc](new ValidationException("at most one table is allowed."))
        else
          for (_ <- validateAll(tables, validateTableType))
            yield (sec.id, tpes, imports, codes, ctx.copy[F](tables = ctx.tables ++ tables))
    }

    override val memoriesTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Memories(mems)) =>
        if (ctx.mems.size + mems.size > 1)
          F.raiseError[Acc](new ValidationException("at most one memory is allowed."))
        else
          for (_ <- validateAll(mems, validateMemType))
            yield (sec.id, tpes, imports, codes, ctx.copy[F](mems = ctx.mems ++ mems))
    }

    override val globalsTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Globals(globals)) =>
        for (_ <- validateAll(globals, EmptyContext[F].copy[F](globals = imported.globals(imports)), validateGlobal))
          yield (sec.id, tpes, imports, codes, ctx.copy[F](globals = ctx.globals ++ globals.map(_.tpe)))
    }

    override val exportsTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Exports(exports)) =>
        val duplicate = exports
          .groupBy(_.fieldName)
          .view
          .mapValues(_.size)
          .find(_._2 > 1)
        duplicate match {
          case Some((name, _)) =>
            F.raiseError[Acc](new ValidationException(s"duplicate export name $name."))
          case None =>
            for (_ <- validateAll(exports, ctx, validateExport))
              yield (sec.id, tpes, imports, codes, ctx)
        }
    }

    override val startTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Start(start)) =>
        for (_ <- validateStart(start, ctx))
          yield (sec.id, tpes, imports, codes, ctx)
    }

    override val elementsTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Elements(elem)) =>
        for (_ <- validateAll(elem, ctx, validateElem))
          yield (sec.id, tpes, imports, codes, ctx)
    }

    override val codeTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Code(code)) =>
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
            yield (sec.id, tpes, imports, code.size, ctx)
        }
    }

    override val datasTraverse = {
      case ((idx, tpes, imports, codes, ctx), sec @ Section.Datas(data)) =>
        for (_ <- validateAll(data, ctx, validateData))
          yield (sec.id, tpes, imports, codes, ctx)
    }

  }

  def validate(stream: Stream[F, Section]): Stream[F, Section] =
    // emit a `None` at the end to allow us to perform last checks
    stream.noneTerminate
      .evalMapAccumulate((0, Vector.empty[Int], Vector.empty[Import], 0, EmptyContext[F])) {
        case (acc @ (idx, _, _, _, _), Some(sec)) =>
          if (sec.id > 0 && sec.id <= idx)
            F.raiseError[(Acc, Option[Section])](
              new ValidationException(s"${nameOf(sec.id)} section may not appear after ${nameOf(idx)} section")
            )
          else
            straverser.traverse(acc, sec).map(_ -> sec.some)
        case (acc @ (_, tpes, _, codes, _), None) =>
          if (tpes.size != codes)
            F.raiseError[(Acc, Option[Section])](
              new ValidationException("code and function sections must have the same number of elements"))
          else
            F.pure((acc, Option.empty[Section]))
      }
      .map(_._2)
      .unNoneTerminate

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
