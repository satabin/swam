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
package compiler

import syntax._

import fs2._

import scala.collection.immutable.VectorBuilder

import scala.language.higherKinds

/** Validates and compiles a module.
  */
class Compiler[F[_]] {

  case class Context(types: Vector[FuncType] = Vector.empty,
    funcs: Vector[Int] = Vector.empty,
    tables: Vector[TableType] = Vector.empty,
    mems: Vector[MemType] = Vector.empty,
    globals: Vector[GlobalType] = Vector.empty,
    val exports: VectorBuilder[runtime.Export] = new VectorBuilder,
    val imports: VectorBuilder[runtime.Import] = new VectorBuilder,
    val customs: VectorBuilder[runtime.Custom] = new VectorBuilder)

  def compile(sections: Stream[F, Section]): Stream[F, runtime.Module] =
    sections.fold(new Context()) {
      case (ctx, Section.Imports(is)) =>
        ctx.imports ++= is.map(toRuntime(ctx.types))
        ctx
      case (ctx, Section.Functions(funcs)) =>
        ctx.copy(funcs = funcs)
      case (ctx, Section.Tables(tables)) =>
        ctx.copy(tables = tables)
      case (ctx, Section.Memories(mems)) =>
        ctx.copy(mems = mems)
      case (ctx, Section.Globals(globals)) =>
        ctx.copy(globals = globals.map(_.tpe))
      case (ctx, Section.Exports(es)) =>
        ctx.exports ++= es.map(toRuntime(ctx))
        ctx
      case (ctx, c @ Section.Custom(_, _)) =>
        ctx.customs += toRuntime(c)
        ctx
      case (ctx, Section.Types(types)) =>
        ctx.copy(types = types)
      case (ctx, sec) =>
        // TODO perform actual compilation
        ctx
    }
    .map(ctx => runtime.Module(ctx.exports.result, ctx.imports.result, ctx.customs.result))

  private def toRuntime(types: Vector[FuncType])(i: Import): runtime.Import =
    i match {
      case Import.Function(mod, fld, tpe) => runtime.Import.Function(mod, fld, types(tpe))
      case Import.Table(mod, fld, tpe) => runtime.Import.Table(mod, fld, tpe)
      case Import.Memory(mod, fld, tpe) => runtime.Import.Memory(mod, fld, tpe)
      case Import.Global(mod, fld, tpe) => runtime.Import.Global(mod, fld, tpe)
    }

  private def toRuntime(ctx: Context)(e: Export): runtime.Export =
    e match {
      case Export(fld, ExternalKind.Function, idx) => runtime.Export.Function(fld, ctx.types(ctx.funcs(idx)))
      case Export(fld, ExternalKind.Table, idx) => runtime.Export.Table(fld, ctx.tables(idx))
      case Export(fld, ExternalKind.Memory, idx) => runtime.Export.Memory(fld, ctx.mems(idx))
      case Export(fld, ExternalKind.Global, idx) => runtime.Export.Global(fld, ctx.globals(idx))
    }

  private def toRuntime(c: Section.Custom): runtime.Custom =
    runtime.Custom(c.name, c.payload)

}
