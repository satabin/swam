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

import fs2._

/** A binary section stream parser.
  * The parser uses the validator to validate the stream.
  */
class ModuleParser[F[_]](validator: Validator[F])(implicit F: Sync[F]) {

  /** Parses a section stream into a module.
    *  When this method returns, the stream has not been begin execution,
    *  you must call one of the `Sync` run method on the result to start actual parsing.
    */
  def parse(stream: Stream[F, Section]): F[Module] =
    stream
      .through(validator.validate)
      // compile the section stream
      .compile
      // accept sections in order
      .fold(EmptyModule) { (mod, sec) =>
        sec match {
          case Section.Types(functypes) =>
            mod.copy(types = functypes)
          case Section.Imports(imports) =>
            mod.copy(imports = imports)
          case Section.Functions(tpes) =>
            mod
          case Section.Tables(tables) =>
            mod.copy(tables = tables)
          case Section.Memories(mems) =>
            mod.copy(mems = mems)
          case Section.Globals(globals) =>
            mod.copy(globals = globals)
          case Section.Exports(exports) =>
            mod.copy(exports = exports)
          case Section.Start(start) =>
            mod.copy(start = Some(start))
          case Section.Elements(elem) =>
            mod.copy(elem = elem)
          case Section.Code(code) =>
            val funcs = code.zip(mod.funcs.indices).map {
              case (FuncBody(locals, code), typeIdx) =>
                val locs = locals.flatMap {
                  case LocalEntry(count, tpe) =>
                    Vector.fill(count)(tpe)
                }
                Func(typeIdx, locs, code)
            }
            mod.copy(funcs = funcs)
          case Section.Datas(data) =>
            mod.copy(data = data)
          case Section.Custom(_, _) =>
            // ignore the custom sections
            mod
        }
      }

}
