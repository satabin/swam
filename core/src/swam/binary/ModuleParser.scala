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
package binary

import syntax._
import validation._

import cats.effect._
import cats.implicits._

import fs2._

import scala.language.higherKinds

/** A binary section stream parser.
  *  The parser uses the validator to validate the stream.
  *  If no validation is wished, use the [[validation.NoopValidator NoopValidator]].
  */
class SwamParser[F[_]](implicit F: Sync[F]) {

  /** Parses a section stream into a module.
    *  When this method returns, the stream has not been begin execution,
    *  you must call one of the `Sync` run method on the result to start actual parsing.
    */
  def parse(stream: Stream[F, Section], validator: Validator[F] = new SpecValidator[F]): F[Module] =
    stream
    // compile the section stream
    .compile
    // accept sections in order
      .fold(F.pure((0, Vector.empty[TypeIdx], EmptyModule, EmptyContext[F]))) { (acc, sec) =>
        acc.flatMap {
          case (idx, tpes, mod, ctx) =>
            if (sec.id > 0 && sec.id <= idx)
              F.raiseError(
                new BinaryException(s"${nameOf(sec.id)} section may not appear after ${nameOf(idx)} section")
              )
            else
              sec match {
                case Section.Types(functypes) =>
                  for (_ <- validator.validateAll(functypes, validator.validateFuncType))
                    yield (sec.id, tpes, mod.copy(types = functypes), ctx.copy(types = functypes))
                case Section.Imports(imports) =>
                  for (_ <- validator.validateAll(imports, ctx, validator.validateImport))
                    yield {
                      val mod1 = mod.copy(imports = imports)
                      val ctx1 = ctx.copy[F](funcs = mod1.imported.funcs,
                                             tables = mod1.imported.tables,
                                             mems = mod1.imported.mems,
                                             globals = mod1.imported.globals)
                      (sec.id, tpes, mod1, ctx1)
                    }
                case Section.Functions(tpes) =>
                  F.pure {
                    val funcs = tpes.map(mod.types(_))
                    (sec.id, tpes, mod, ctx.copy(funcs = ctx.funcs ++ funcs))
                  }
                case Section.Tables(tables) =>
                  if (tables.size > 1)
                    F.raiseError(new ValidationException("at most one table is allowed."))
                  else
                    for (_ <- validator.validateAll(tables, validator.validateTableType))
                      yield (sec.id, tpes, mod.copy(tables = tables), ctx.copy(tables = ctx.tables ++ tables))
                case Section.Memories(mems) =>
                  if (mems.size > 1)
                    F.raiseError(new ValidationException("at most one memory is allowed."))
                  else
                    for (_ <- validator.validateAll(mems, validator.validateMemType))
                      yield (sec.id, tpes, mod.copy(mems = mems), ctx.copy(mems = ctx.mems ++ mems))
                case Section.Globals(globals) =>
                  for (_ <- validator.validateAll(globals,
                                                  EmptyContext[F].copy(globals = mod.imported.globals),
                                                  validator.validateGlobal))
                    yield
                      (sec.id, tpes, mod.copy(globals = globals), ctx.copy(globals = ctx.globals ++ globals.map(_.tpe)))
                case Section.Exports(exports) =>
                  val duplicate = exports
                    .groupBy(_.fieldName)
                    .mapValues(_.size)
                    .find(_._2 > 1)
                  duplicate match {
                    case Some((name, _)) =>
                      F.raiseError(new ValidationException(s"duplicate export name $name."))
                    case None =>
                      for (_ <- validator.validateAll(exports, ctx, validator.validateExport))
                        yield (sec.id, tpes, mod.copy(exports = exports), ctx)
                  }
                case Section.Start(start) =>
                  for (_ <- validator.validateStart(start, ctx))
                    yield (sec.id, tpes, mod.copy(start = Some(start)), ctx)
                case Section.Elements(elem) =>
                  for (_ <- validator.validateAll(elem, ctx, validator.validateElem))
                    yield (sec.id, tpes, mod.copy(elem = elem), ctx)
                case Section.Code(code) =>
                  if (code.size != tpes.size)
                    F.raiseError(
                      new BinaryException("code and function sections must have the same number of elements")
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
                    for (_ <- validator.validateAll(funcs, ctx, validator.validateFunction))
                      yield (sec.id, Vector.empty, mod.copy(funcs = funcs), ctx)
                  }
                case Section.Datas(data) =>
                  for (_ <- validator.validateAll(data, ctx, validator.validateData))
                    yield (sec.id, tpes, mod.copy(data = data), ctx)
                case Section.Custom(_, _) =>
                  // ignore the custom sections
                  F.pure((idx, tpes, mod, ctx))
              }
        }
      }
      .flatten
      // drop the section index and type indexes, just to keep the compiled module
      .map(_._3)

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
