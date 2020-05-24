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
package decompilation

import syntax._
import validation._
import text.{unresolved => u}
import text.unresolved.pretty._
import util.pretty._

import binary.custom._

import cats.effect._
import cats.implicits._

import fs2._

import scodec._

import scala.annotation.tailrec

/** A decompiler from binary format, that formats into the text representation.
  *
  * This decompiler requires a valid module (validation is performed by the decompiler)
  * to be able to work. It uses the fact that all types are valid to smartly use
  * the folded syntax for all operators and function calls.
  *
  * This decompiler also takes advantage of the custom name section if present
  * to add identifier to the output.
  */
class TextDecompiler[F[_]] private (validator: Validator[F])(implicit F: Sync[F]) extends Decompiler[F] {

  private def pretty(m: u.Module, functypes: Map[u.Index, FuncType]): Doc = {
    // take advantage of the fact that the module is valid
    // hence we can pretty-fold the function calls
    implicit object ExprPretty extends Pretty[u.Expr] {
      def pretty(e: u.Expr): Doc = {
        @tailrec
        def loop(e: u.Expr, acc: List[Doc]): Doc =
          (e, acc) match {
            case (Seq(), _) => seq(line, acc.reverse)
            case (Seq(op @ u.Binop(_), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op @ u.Unop(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ u.Testop(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ u.Relop(_), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op @ u.Convertop(_, _), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(i @ u.If(_, _, _, _, _, _), rest @ _*), d :: restd) =>
              loop(rest, foldedif(i, d) :: restd)
            case (Seq(i @ u.Call(_), rest @ _*), _) =>
              loop(rest, foldedcall(i, acc))
            case (Seq(op @ u.BrIf(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ u.LocalSet(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ u.LocalTee(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ u.GlobalSet(_), rest @ _*), d :: restd) =>
              loop(rest, unop(op, d) :: restd)
            case (Seq(op @ u.Store(_, _, _), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op @ u.StoreN(_, _, _, _), rest @ _*), d2 :: d1 :: restd) =>
              loop(rest, binop(op, d1, d2) :: restd)
            case (Seq(op, rest @ _*), _) =>
              loop(rest, op.pretty :: acc)
          }
        loop(e, Nil)
      }

      def foldedcall(c: u.Call, acc: List[Doc]): List[Doc] = {
        val tpe = functypes(c.funcidx)
        val nbparams = tpe.params.size
        val (args, rest) = acc.splitAt(nbparams)
        val pargs = seq(line, args.reverse.map(a => fold(a.pretty, empty)))
        group(nest(2, str("(") ++ c.pretty ++ line ++ pargs ++ str(")"))) :: rest
      }
    }
    m.pretty
  }

  /** Decompiles to an [[swam.text.unresolved.Module unresolved text module]]. */
  def decompileModule(sections: Stream[F, Section]): F[(u.Module, Map[u.Index, FuncType])] =
    sections
      .through(validator.validate(_))
      .evalScan(DecompilerEnv()) {
        case (env, Section.Types(types)) =>
          F.pure(env.copy(types = types))
        case (env, Section.Imports(imports)) =>
          F.pure(env.copy(imports = imports))
        case (env, Section.Functions(functions)) =>
          F.pure(env.copy(functions = functions))
        case (env, Section.Tables(tables)) =>
          F.pure(env.copy(tables = tables))
        case (env, Section.Memories(memories)) =>
          F.pure(env.copy(memories = memories))
        case (env, Section.Globals(globals)) =>
          F.pure(env.copy(globals = globals))
        case (env, Section.Exports(exports)) =>
          F.pure(env.copy(exports = exports))
        case (env, Section.Start(start)) =>
          F.pure(env.copy(start = Some(start)))
        case (env, Section.Elements(elements)) =>
          F.pure(env.copy(elements = elements))
        case (env, Section.Code(code)) =>
          F.pure(env.copy(code = code))
        case (env, Section.Datas(data)) =>
          F.pure(env.copy(data = data))
        case (env, Section.Custom("name", payload)) =>
          NameSectionHandler.codec.decodeValue(payload) match {
            case Attempt.Successful(Names(subsections)) =>
              F.pure(subsections.foldLeft(env) {
                case (env, ModuleName(name))     => env.copy(moduleName = Some(name))
                case (env, FunctionNames(names)) => env.copy(functionNames = names)
                case (env, LocalNames(names)) =>
                  env.copy(localNames = names.flatMap {
                    case (fidx, locals) =>
                      locals.map {
                        case (lidx, n) => ((fidx, lidx), n)
                      }
                  }.toMap)
                case (env, LabelNames(names)) =>
                  env.copy(labelNames = names.flatMap {
                    case (fidx, labels) =>
                      labels.map {
                        case (lidx, n) => ((fidx, lidx), n)
                      }
                  }.toMap)
                case (env, TypeNames(names))   => env.copy(typeNames = names)
                case (env, TableNames(names))  => env.copy(tableNames = names)
                case (env, MemoryNames(names)) => env.copy(memoryNames = names)
                case (env, GlobalNames(names)) => env.copy(globalNames = names)
              })
            case Attempt.Failure(f) =>
              F.raiseError(new DecompilerException(f.message))
          }
        case (env, s) =>
          F.pure(env)
      }
      .compile
      .last
      .map {
        case Some(env) =>
          // we first decompile the imports
          val (imports, fidx, tidx, midx, gidx) = decompile(env.imports,
                                                            env.types,
                                                            env.functionNames,
                                                            env.tableNames,
                                                            env.memoryNames,
                                                            env.globalNames,
                                                            Seq.empty,
                                                            0,
                                                            0,
                                                            0,
                                                            0)
          // this gives us the base index for functions, tables, memories, and globals
          // defined in this module
          // this is useful to get proper name from the function names section
          // but first let's start with the exports
          val exports = decompile(env.exports, env.functionNames, env.tableNames, env.memoryNames, env.globalNames)
          // then let's define the memories that are used
          val memories = decompileMems(env.memories, env.memoryNames, midx)
          // followed by the data
          val data = decompileData(env.data, env.types, env.functionNames, env.localNames, env.labelNames)
          // then come the table declarations
          val tables = decompileTables(env.tables, env.tableNames, tidx)
          // then the global declarations
          val globals =
            decompileGlobals(env.globals,
                             env.globalNames,
                             gidx,
                             env.types,
                             env.functionNames,
                             env.localNames,
                             env.labelNames)
          // closely followed by their initialization
          val elems = decompileElems(env.elements, env.types, env.functionNames, env.localNames, env.labelNames)
          // the start element
          val start = decompileStart(env.start, env.functionNames)
          // and finally, at last, the functions
          val functions =
            decompileFunctions(env.functions.map(env.types(_)).zip(env.code),
                               env.types,
                               env.functionNames,
                               env.localNames,
                               env.labelNames,
                               fidx)
          // build the map from index to type
          val types = imports
            .foldLeft((0, List.empty[(u.Index, FuncType)])) {
              case ((idx, acc), u.Import(_, _, u.ImportDesc.Func(id, u.TypeUse(_, ps, rs)))) =>
                val f = FuncType(ps.map(_._2), rs)
                val acc1 = id.toOption match {
                  case Some(_) => (Right(id) -> f) :: (Left(idx) -> f) :: acc
                  case None    => (Left(idx) -> f) :: acc
                }
                (idx + 1, acc1)
              case (acc, _) => acc
            }
            ._2 ++ functions.zipWithIndex.flatMap {
            case (u.Function(id, u.TypeUse(_, ps, rs), _, _), idx) =>
              val f = FuncType(ps.map(_._2), rs)
              id.toOption match {
                case Some(_) => List(Right(id) -> f, Left(idx + fidx) -> f)
                case None    => List(Left(idx + fidx) -> f)
              }
          }
          val id = u.Id.fromOption(env.moduleName)
          val functypes = types.toMap
          (u.Module(id, imports ++ exports ++ globals ++ memories ++ data ++ tables ++ elems ++ start ++ functions)(-1),
           functypes)
        case None =>
          (u.Module(u.NoId, Seq.empty)(-1), Map.empty)
      }

  def decompile(sections: Stream[F, Section]): F[Doc] =
    decompileModule(sections).map((pretty _).tupled)

  @tailrec
  private def decompile(imports: Seq[Import],
                        types: Vector[FuncType],
                        functionNames: Map[Int, String],
                        tableNames: Map[Int, String],
                        memoryNames: Map[Int, String],
                        globalNames: Map[Int, String],
                        acc: Seq[u.Import],
                        fidx: Int,
                        tidx: Int,
                        midx: Int,
                        gidx: Int): (Seq[u.Import], Int, Int, Int, Int) =
    imports match {
      case Seq() =>
        (acc.reverse, fidx, tidx, midx, gidx)
      case Seq(Import.Function(mod, name, tpe), rest @ _*) =>
        val fname = u.Id.fromOption(functionNames.get(fidx))
        val ftpe = types(tpe)
        val params = ftpe.params.map(u.NoId -> _)
        val imp = u.Import(mod, name, u.ImportDesc.Func(fname, u.TypeUse(None, params, ftpe.t))(-1))(-1)
        decompile(rest,
                  types,
                  functionNames,
                  tableNames,
                  memoryNames,
                  globalNames,
                  imp +: acc,
                  fidx + 1,
                  tidx,
                  midx,
                  gidx)
      case Seq(Import.Table(mod, name, tpe), rest @ _*) =>
        val tname = u.Id.fromOption(tableNames.get(tidx))
        val imp = u.Import(mod, name, u.ImportDesc.Table(tname, tpe)(-1))(-1)
        decompile(rest,
                  types,
                  functionNames,
                  tableNames,
                  memoryNames,
                  globalNames,
                  imp +: acc,
                  fidx,
                  tidx + 1,
                  midx,
                  gidx)
      case Seq(Import.Memory(mod, name, tpe), rest @ _*) =>
        val mname = u.Id.fromOption(memoryNames.get(midx))
        val imp = u.Import(mod, name, u.ImportDesc.Memory(mname, tpe)(-1))(-1)
        decompile(rest,
                  types,
                  functionNames,
                  tableNames,
                  memoryNames,
                  globalNames,
                  imp +: acc,
                  fidx,
                  tidx,
                  midx + 1,
                  gidx)
      case Seq(Import.Global(mod, name, tpe), rest @ _*) =>
        val gname = u.Id.fromOption(globalNames.get(gidx))
        val imp = u.Import(mod, name, u.ImportDesc.Global(gname, tpe)(-1))(-1)
        decompile(rest,
                  types,
                  functionNames,
                  tableNames,
                  memoryNames,
                  globalNames,
                  imp +: acc,
                  fidx,
                  tidx,
                  midx,
                  gidx + 1)
    }

  private def decompile(exports: Seq[Export],
                        functionNames: Map[Int, String],
                        tableNames: Map[Int, String],
                        memoryNames: Map[Int, String],
                        globalNames: Map[Int, String]): Seq[u.Export] =
    exports.map {
      case Export(name, ExternalKind.Function, idx) =>
        val fid = u.Id.fromOptionOrElse(functionNames.get(idx), idx)
        u.Export(name, u.ExportDesc.Func(fid)(-1))(-1)
      case Export(name, ExternalKind.Table, idx) =>
        val tid = u.Id.fromOptionOrElse(tableNames.get(idx), idx)
        u.Export(name, u.ExportDesc.Table(tid)(-1))(-1)
      case Export(name, ExternalKind.Memory, idx) =>
        val mid = u.Id.fromOptionOrElse(memoryNames.get(idx), idx)
        u.Export(name, u.ExportDesc.Memory(mid)(-1))(-1)
      case Export(name, ExternalKind.Global, idx) =>
        val gid = u.Id.fromOptionOrElse(globalNames.get(idx), idx)
        u.Export(name, u.ExportDesc.Global(gid)(-1))(-1)
    }

  private def decompileMems(mems: Seq[MemType], memoryNames: Map[Int, String], midx: Int): Seq[u.Memory] =
    mems.zipWithIndex.map {
      case (mem, idx) =>
        val id = u.Id.fromOption(memoryNames.get(idx + midx))
        u.Memory(id, mem)(-1)
    }

  private def decompileData(data: Vector[Data],
                            functypes: Vector[FuncType],
                            functionNames: Map[Int, String],
                            localNames: Map[(Int, Int), String],
                            labelNames: Map[(Int, Int), String]): Seq[u.Data] =
    data.map {
      case Data(idx, offset, init) =>
        u.Data(Left(idx),
               decompileExpr(0, offset, -1, functypes, functionNames, localNames, labelNames)._2,
               init.toByteArray)(-1)
    }

  private def decompileTables(tables: Seq[TableType], tableNames: Map[Int, String], tidx: Int): Seq[u.Table] =
    tables.zipWithIndex.map {
      case (table, idx) =>
        val id = u.Id.fromOption(tableNames.get(idx + tidx))
        u.Table(id, table)(-1)
    }

  private def decompileGlobals(globals: Seq[Global],
                               globalNames: Map[Int, String],
                               gidx: Int,
                               functypes: Vector[FuncType],
                               functionNames: Map[Int, String],
                               localNames: Map[(Int, Int), String],
                               labelNames: Map[(Int, Int), String]): Seq[u.Global] =
    globals.zipWithIndex.map {
      case (Global(tpe, init), idx) =>
        val id = u.Id.fromOption(globalNames.get(idx + gidx))
        val expr = decompileExpr(0, init, -1, functypes, functionNames, localNames, labelNames)._2
        u.Global(id, tpe, expr)(-1)
    }

  private def decompileElems(elems: Vector[Elem],
                             functypes: Vector[FuncType],
                             functionNames: Map[Int, String],
                             localNames: Map[(Int, Int), String],
                             labelNames: Map[(Int, Int), String]): Seq[u.Elem] =
    elems.map {
      case Elem(idx, offset, init) =>
        val funs = init.map(idx => u.Id.fromOptionOrElse(functionNames.get(idx), idx))
        u.Elem(Left(idx), decompileExpr(0, offset, -1, functypes, functionNames, localNames, labelNames)._2, funs)(-1)
    }

  private def decompileStart(s: Option[FuncIdx], functionNames: Map[Int, String]): Seq[u.StartFunc] =
    s match {
      case Some(idx) =>
        val id = u.Id.fromOptionOrElse(functionNames.get(idx), idx)
        Seq(u.StartFunc(id)(-1))
      case None =>
        Seq.empty
    }

  private def decompileFunctions(functions: Seq[(FuncType, FuncBody)],
                                 functypes: Vector[FuncType],
                                 functionNames: Map[Int, String],
                                 localNames: Map[(Int, Int), String],
                                 labelNames: Map[(Int, Int), String],
                                 fidx: Int): Seq[u.Function] =
    functions.zipWithIndex.map {
      case ((tpe, FuncBody(locals, code)), idx) =>
        val gidx = idx + fidx
        val id = u.Id.fromOption(functionNames.get(gidx))
        val params = tpe.params.zipWithIndex.map {
          case (tpe, i) =>
            (u.Id.fromOption(localNames.get((gidx, i))), tpe)
        }
        val nbparams = params.size
        val tu = u.TypeUse(None, params, tpe.t)
        @tailrec
        def makeLocals(idx: Int, locals: Seq[LocalEntry], acc: Seq[u.Local]): Seq[u.Local] =
          locals match {
            case Seq() =>
              acc.reverse
            case Seq(LocalEntry(count, tpe), rest @ _*) =>
              val ls = (count - 1 to 0).map { i =>
                val id = u.Id.fromOption(localNames.get((gidx, idx + i)))
                u.Local(id, tpe)(-1)
              }
              makeLocals(idx + count, rest, ls ++ acc)
          }
        val ls = makeLocals(nbparams, locals, Seq.empty)
        val is = decompileExpr(0, code, gidx, functypes, functionNames, localNames, labelNames)._2
        u.Function(id, tu, ls, is)(-1)
    }

  private def decompileExpr(lblidx: Int,
                            expr: Expr,
                            fidx: Int,
                            functypes: Vector[FuncType],
                            functionNames: Map[Int, String],
                            localNames: Map[(Int, Int), String],
                            labelNames: Map[(Int, Int), String]): (Int, u.Expr) =
    expr
      .foldLeft((lblidx, Vector.empty[u.Inst])) {
        case ((lblidx, acc), inst) =>
          val (lblidx1, inst1) =
            decompileInstruction(lblidx, inst, fidx, functypes, functionNames, localNames, labelNames)
          (lblidx1, acc :+ inst1)
      }

  private def decompileInstruction(lblidx: Int,
                                   i: Inst,
                                   fidx: Int,
                                   functypes: Vector[FuncType],
                                   functionNames: Map[Int, String],
                                   localNames: Map[(Int, Int), String],
                                   labelNames: Map[(Int, Int), String]): (Int, u.Inst) = {
    def getLocalId(idx: Int) =
      u.Id.fromOptionOrElse(localNames.get((fidx, idx)), idx)
    def getLabelId(idx: Int) =
      u.Id.fromOption(labelNames.get((fidx, idx)))
    def getLabelIdOrIdx(idx: Int) =
      u.Id.fromOptionOrElse(labelNames.get((fidx, idx)), idx)
    def getFunId(idx: Int) =
      u.Id.fromOptionOrElse(functionNames.get(idx), idx)
    i match {
      case i32.Const(v)       => (lblidx, u.i32.Const(v)(-1))
      case i32.Clz            => (lblidx, u.i32.Clz()(-1))
      case i32.Ctz            => (lblidx, u.i32.Ctz()(-1))
      case i32.Popcnt         => (lblidx, u.i32.Popcnt()(-1))
      case i32.Extend8S       => (lblidx, u.i32.Extend8S()(-1))
      case i32.Extend16S      => (lblidx, u.i32.Extend16S()(-1))
      case i32.Add            => (lblidx, u.i32.Add()(-1))
      case i32.Sub            => (lblidx, u.i32.Sub()(-1))
      case i32.Mul            => (lblidx, u.i32.Mul()(-1))
      case i32.DivS           => (lblidx, u.i32.DivS()(-1))
      case i32.DivU           => (lblidx, u.i32.DivU()(-1))
      case i32.RemS           => (lblidx, u.i32.RemS()(-1))
      case i32.RemU           => (lblidx, u.i32.RemU()(-1))
      case i32.And            => (lblidx, u.i32.And()(-1))
      case i32.Or             => (lblidx, u.i32.Or()(-1))
      case i32.Xor            => (lblidx, u.i32.Xor()(-1))
      case i32.Shl            => (lblidx, u.i32.Shl()(-1))
      case i32.ShrS           => (lblidx, u.i32.ShrS()(-1))
      case i32.ShrU           => (lblidx, u.i32.ShrU()(-1))
      case i32.Rotl           => (lblidx, u.i32.Rotl()(-1))
      case i32.Rotr           => (lblidx, u.i32.Rotr()(-1))
      case i32.Eqz            => (lblidx, u.i32.Eqz()(-1))
      case i32.Eq             => (lblidx, u.i32.Eq()(-1))
      case i32.Ne             => (lblidx, u.i32.Ne()(-1))
      case i32.LtS            => (lblidx, u.i32.LtS()(-1))
      case i32.LtU            => (lblidx, u.i32.LtU()(-1))
      case i32.GtS            => (lblidx, u.i32.GtS()(-1))
      case i32.GtU            => (lblidx, u.i32.GtU()(-1))
      case i32.LeS            => (lblidx, u.i32.LeS()(-1))
      case i32.LeU            => (lblidx, u.i32.LeU()(-1))
      case i32.GeS            => (lblidx, u.i32.GeS()(-1))
      case i32.GeU            => (lblidx, u.i32.GeU()(-1))
      case i32.WrapI64        => (lblidx, u.i32.WrapI64()(-1))
      case i32.TruncSF32      => (lblidx, u.i32.TruncSF32()(-1))
      case i32.TruncUF32      => (lblidx, u.i32.TruncUF32()(-1))
      case i32.TruncSF64      => (lblidx, u.i32.TruncSF64()(-1))
      case i32.TruncUF64      => (lblidx, u.i32.TruncUF64()(-1))
      case i32.TruncSatSF32   => (lblidx, u.i32.TruncSatSF32()(-1))
      case i32.TruncSatUF32   => (lblidx, u.i32.TruncSatUF32()(-1))
      case i32.TruncSatSF64   => (lblidx, u.i32.TruncSatSF64()(-1))
      case i32.TruncSatUF64   => (lblidx, u.i32.TruncSatUF64()(-1))
      case i32.ReinterpretF32 => (lblidx, u.i32.ReinterpretF32()(-1))
      case i32.Load(a, o)     => (lblidx, u.i32.Load(a, o)(-1))
      case i32.Store(a, o)    => (lblidx, u.i32.Store(a, o)(-1))
      case i32.Load8S(a, o)   => (lblidx, u.i32.Load8S(a, o)(-1))
      case i32.Load8U(a, o)   => (lblidx, u.i32.Load8U(a, o)(-1))
      case i32.Load16S(a, o)  => (lblidx, u.i32.Load16S(a, o)(-1))
      case i32.Load16U(a, o)  => (lblidx, u.i32.Load16U(a, o)(-1))
      case i32.Store8(a, o)   => (lblidx, u.i32.Store8(a, o)(-1))
      case i32.Store16(a, o)  => (lblidx, u.i32.Store16(a, o)(-1))
      case i64.Const(v)       => (lblidx, u.i64.Const(v)(-1))
      case i64.Clz            => (lblidx, u.i64.Clz()(-1))
      case i64.Ctz            => (lblidx, u.i64.Ctz()(-1))
      case i64.Popcnt         => (lblidx, u.i64.Popcnt()(-1))
      case i64.Extend8S       => (lblidx, u.i64.Extend8S()(-1))
      case i64.Extend16S      => (lblidx, u.i64.Extend16S()(-1))
      case i64.Extend32S      => (lblidx, u.i64.Extend32S()(-1))
      case i64.Add            => (lblidx, u.i64.Add()(-1))
      case i64.Sub            => (lblidx, u.i64.Sub()(-1))
      case i64.Mul            => (lblidx, u.i64.Mul()(-1))
      case i64.DivS           => (lblidx, u.i64.DivS()(-1))
      case i64.DivU           => (lblidx, u.i64.DivU()(-1))
      case i64.RemS           => (lblidx, u.i64.RemS()(-1))
      case i64.RemU           => (lblidx, u.i64.RemU()(-1))
      case i64.And            => (lblidx, u.i64.And()(-1))
      case i64.Or             => (lblidx, u.i64.Or()(-1))
      case i64.Xor            => (lblidx, u.i64.Xor()(-1))
      case i64.Shl            => (lblidx, u.i64.Shl()(-1))
      case i64.ShrS           => (lblidx, u.i64.ShrS()(-1))
      case i64.ShrU           => (lblidx, u.i64.ShrU()(-1))
      case i64.Rotl           => (lblidx, u.i64.Rotl()(-1))
      case i64.Rotr           => (lblidx, u.i64.Rotr()(-1))
      case i64.Eqz            => (lblidx, u.i64.Eqz()(-1))
      case i64.Eq             => (lblidx, u.i64.Eq()(-1))
      case i64.Ne             => (lblidx, u.i64.Ne()(-1))
      case i64.LtS            => (lblidx, u.i64.LtS()(-1))
      case i64.LtU            => (lblidx, u.i64.LtU()(-1))
      case i64.GtS            => (lblidx, u.i64.GtS()(-1))
      case i64.GtU            => (lblidx, u.i64.GtU()(-1))
      case i64.LeS            => (lblidx, u.i64.LeS()(-1))
      case i64.LeU            => (lblidx, u.i64.LeU()(-1))
      case i64.GeS            => (lblidx, u.i64.GeS()(-1))
      case i64.GeU            => (lblidx, u.i64.GeU()(-1))
      case i64.ExtendSI32     => (lblidx, u.i64.ExtendSI32()(-1))
      case i64.ExtendUI32     => (lblidx, u.i64.ExtendUI32()(-1))
      case i64.TruncSF32      => (lblidx, u.i64.TruncSF32()(-1))
      case i64.TruncUF32      => (lblidx, u.i64.TruncUF32()(-1))
      case i64.TruncSF64      => (lblidx, u.i64.TruncSF64()(-1))
      case i64.TruncUF64      => (lblidx, u.i64.TruncUF64()(-1))
      case i64.TruncSatSF32   => (lblidx, u.i64.TruncSatSF32()(-1))
      case i64.TruncSatUF32   => (lblidx, u.i64.TruncSatUF32()(-1))
      case i64.TruncSatSF64   => (lblidx, u.i64.TruncSatSF64()(-1))
      case i64.TruncSatUF64   => (lblidx, u.i64.TruncSatUF64()(-1))
      case i64.ReinterpretF64 => (lblidx, u.i64.ReinterpretF64()(-1))
      case i64.Load(a, o)     => (lblidx, u.i64.Load(a, o)(-1))
      case i64.Store(a, o)    => (lblidx, u.i64.Store(a, o)(-1))
      case i64.Load8S(a, o)   => (lblidx, u.i64.Load8S(a, o)(-1))
      case i64.Load8U(a, o)   => (lblidx, u.i64.Load8U(a, o)(-1))
      case i64.Load16S(a, o)  => (lblidx, u.i64.Load16S(a, o)(-1))
      case i64.Load16U(a, o)  => (lblidx, u.i64.Load16U(a, o)(-1))
      case i64.Load32S(a, o)  => (lblidx, u.i64.Load32S(a, o)(-1))
      case i64.Load32U(a, o)  => (lblidx, u.i64.Load32U(a, o)(-1))
      case i64.Store8(a, o)   => (lblidx, u.i64.Store8(a, o)(-1))
      case i64.Store16(a, o)  => (lblidx, u.i64.Store16(a, o)(-1))
      case i64.Store32(a, o)  => (lblidx, u.i64.Store32(a, o)(-1))
      case f32.Const(v)       => (lblidx, u.f32.Const(v)(-1))
      case f32.Abs            => (lblidx, u.f32.Abs()(-1))
      case f32.Neg            => (lblidx, u.f32.Neg()(-1))
      case f32.Sqrt           => (lblidx, u.f32.Sqrt()(-1))
      case f32.Ceil           => (lblidx, u.f32.Ceil()(-1))
      case f32.Floor          => (lblidx, u.f32.Floor()(-1))
      case f32.Trunc          => (lblidx, u.f32.Trunc()(-1))
      case f32.Nearest        => (lblidx, u.f32.Nearest()(-1))
      case f32.Add            => (lblidx, u.f32.Add()(-1))
      case f32.Sub            => (lblidx, u.f32.Sub()(-1))
      case f32.Mul            => (lblidx, u.f32.Mul()(-1))
      case f32.Div            => (lblidx, u.f32.Div()(-1))
      case f32.Min            => (lblidx, u.f32.Min()(-1))
      case f32.Max            => (lblidx, u.f32.Max()(-1))
      case f32.Copysign       => (lblidx, u.f32.Copysign()(-1))
      case f32.Eq             => (lblidx, u.f32.Eq()(-1))
      case f32.Ne             => (lblidx, u.f32.Ne()(-1))
      case f32.Lt             => (lblidx, u.f32.Lt()(-1))
      case f32.Gt             => (lblidx, u.f32.Gt()(-1))
      case f32.Le             => (lblidx, u.f32.Le()(-1))
      case f32.Ge             => (lblidx, u.f32.Ge()(-1))
      case f32.DemoteF64      => (lblidx, u.f32.DemoteF64()(-1))
      case f32.ConvertSI32    => (lblidx, u.f32.ConvertSI32()(-1))
      case f32.ConvertUI32    => (lblidx, u.f32.ConvertUI32()(-1))
      case f32.ConvertSI64    => (lblidx, u.f32.ConvertSI64()(-1))
      case f32.ConvertUI64    => (lblidx, u.f32.ConvertUI64()(-1))
      case f32.ReinterpretI32 => (lblidx, u.f32.ReinterpretI32()(-1))
      case f32.Load(a, o)     => (lblidx, u.f32.Load(a, o)(-1))
      case f32.Store(a, o)    => (lblidx, u.f32.Store(a, o)(-1))
      case f64.Const(v)       => (lblidx, u.f64.Const(v)(-1))
      case f64.Abs            => (lblidx, u.f64.Abs()(-1))
      case f64.Neg            => (lblidx, u.f64.Neg()(-1))
      case f64.Sqrt           => (lblidx, u.f64.Sqrt()(-1))
      case f64.Ceil           => (lblidx, u.f64.Ceil()(-1))
      case f64.Floor          => (lblidx, u.f64.Floor()(-1))
      case f64.Trunc          => (lblidx, u.f64.Trunc()(-1))
      case f64.Nearest        => (lblidx, u.f64.Nearest()(-1))
      case f64.Add            => (lblidx, u.f64.Add()(-1))
      case f64.Sub            => (lblidx, u.f64.Sub()(-1))
      case f64.Mul            => (lblidx, u.f64.Mul()(-1))
      case f64.Div            => (lblidx, u.f64.Div()(-1))
      case f64.Min            => (lblidx, u.f64.Min()(-1))
      case f64.Max            => (lblidx, u.f64.Max()(-1))
      case f64.Copysign       => (lblidx, u.f64.Copysign()(-1))
      case f64.Eq             => (lblidx, u.f64.Eq()(-1))
      case f64.Ne             => (lblidx, u.f64.Ne()(-1))
      case f64.Lt             => (lblidx, u.f64.Lt()(-1))
      case f64.Gt             => (lblidx, u.f64.Gt()(-1))
      case f64.Le             => (lblidx, u.f64.Le()(-1))
      case f64.Ge             => (lblidx, u.f64.Ge()(-1))
      case f64.PromoteF32     => (lblidx, u.f64.PromoteF32()(-1))
      case f64.ConvertSI32    => (lblidx, u.f64.ConvertSI32()(-1))
      case f64.ConvertUI32    => (lblidx, u.f64.ConvertUI32()(-1))
      case f64.ConvertSI64    => (lblidx, u.f64.ConvertSI64()(-1))
      case f64.ConvertUI64    => (lblidx, u.f64.ConvertUI64()(-1))
      case f64.ReinterpretI64 => (lblidx, u.f64.ReinterpretI64()(-1))
      case f64.Load(a, o)     => (lblidx, u.f64.Load(a, o)(-1))
      case f64.Store(a, o)    => (lblidx, u.f64.Store(a, o)(-1))
      case Drop               => (lblidx, u.Drop()(-1))
      case Select             => (lblidx, u.Select()(-1))
      case LocalGet(idx)      => (lblidx, u.LocalGet(getLocalId(idx))(-1))
      case LocalSet(idx)      => (lblidx, u.LocalSet(getLocalId(idx))(-1))
      case LocalTee(idx)      => (lblidx, u.LocalTee(getLocalId(idx))(-1))
      case GlobalGet(idx)     => (lblidx, u.GlobalGet(Left(idx))(-1))
      case GlobalSet(idx)     => (lblidx, u.GlobalSet(Left(idx))(-1))
      case MemorySize         => (lblidx, u.MemorySize()(-1))
      case MemoryGrow         => (lblidx, u.MemoryGrow()(-1))
      case Nop                => (lblidx, u.Nop()(-1))
      case Unreachable        => (lblidx, u.Unreachable()(-1))
      case Block(tpe, is) =>
        val (lblidx1, expr) = decompileExpr(lblidx + 1, is, fidx, functypes, functionNames, localNames, labelNames)
        val id = getLabelId(lblidx)
        (lblidx1, u.Block(id, decompileBlockType(tpe, functypes), expr, id)(-1))
      case Loop(tpe, is) =>
        val (lblidx1, expr) = decompileExpr(lblidx + 1, is, fidx, functypes, functionNames, localNames, labelNames)
        val id = getLabelId(lblidx)
        (lblidx1, u.Loop(id, decompileBlockType(tpe, functypes), expr, id)(-1))
      case If(tpe, theni, elsei) =>
        val (lblidx1, theni1) = decompileExpr(lblidx + 1, theni, fidx, functypes, functionNames, localNames, labelNames)
        val (lblidx2, elsei1) =
          decompileExpr(lblidx1 + 1, elsei, fidx, functypes, functionNames, localNames, labelNames)
        val id = getLabelId(lblidx)
        (lblidx2, u.If(id, decompileBlockType(tpe, functypes), theni1, id, elsei1, id)(-1))
      case Br(label)           => (lblidx, u.Br(getLabelIdOrIdx(label))(-1))
      case BrIf(label)         => (lblidx, u.BrIf(getLabelIdOrIdx(label))(-1))
      case BrTable(table, lbl) => (lblidx, u.BrTable(table.map(getLabelIdOrIdx(_)), getLabelIdOrIdx(lbl))(-1))
      case Return              => (lblidx, u.Return()(-1))
      case Call(lbl)           => (lblidx, u.Call(getFunId(lbl))(-1))
      case CallIndirect(idx) =>
        val ftpe = functypes(idx)
        val params = ftpe.params.map(u.NoId -> _)
        val tu = u.TypeUse(None, params, ftpe.t)
        (lblidx, u.CallIndirect(tu)(-1))
    }

  }

  private def decompileBlockType(bt: BlockType, types: Vector[FuncType]): u.TypeUse =
    bt match {
      case BlockType.NoType         => u.TypeUse(None, Vector.empty, Vector.empty)
      case BlockType.ValueType(tpe) => u.TypeUse(None, Vector.empty, Vector(tpe))
      case BlockType.FunctionType(tpe) =>
        val FuncType(params, ret) = types(tpe)
        u.TypeUse(None, params.map((u.NoId, _)), ret)
    }
}

object TextDecompiler {
  def apply[F[_]: Sync: ContextShift](blocker: Blocker): F[TextDecompiler[F]] =
    for {
      validator <- Validator[F](blocker)
    } yield TextDecompiler[F](validator)

  def apply[F[_]: Sync](validator: Validator[F]): TextDecompiler[F] =
    new TextDecompiler[F](validator)
}

private case class DecompilerEnv(moduleName: Option[String] = None,
                                 functionNames: Map[Int, String] = Map.empty,
                                 localNames: Map[(Int, Int), String] = Map.empty,
                                 labelNames: Map[(Int, Int), String] = Map.empty,
                                 typeNames: Map[Int, String] = Map.empty,
                                 tableNames: Map[Int, String] = Map.empty,
                                 memoryNames: Map[Int, String] = Map.empty,
                                 globalNames: Map[Int, String] = Map.empty,
                                 types: Vector[FuncType] = Vector.empty,
                                 imports: Vector[Import] = Vector.empty,
                                 functions: Vector[Int] = Vector.empty,
                                 tables: Vector[TableType] = Vector.empty,
                                 memories: Vector[MemType] = Vector.empty,
                                 globals: Vector[Global] = Vector.empty,
                                 exports: Vector[Export] = Vector.empty,
                                 start: Option[FuncIdx] = None,
                                 elements: Vector[Elem] = Vector.empty,
                                 code: Vector[FuncBody] = Vector.empty,
                                 data: Vector[Data] = Vector.empty)
