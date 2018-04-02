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

import cats.effect._
import cats.implicits._

import fs2._

import scala.language.higherKinds

import java.io.FileInputStream

/** A binary section stream parser.
 *  The parser may validate the streams as they come if wished.
 */
class SwamParser(validating: Boolean) {

  /** Parses a section stream into a module.
   *  When this method returns, the stream has not been begin execution,
   *  you must call on of the `Sync` run method on the result to start actual parsing.
   */
  def parse[F[_]](stream: Stream[F, Section])(implicit F: Sync[F]): F[Module] =
    stream
      // drop custom sections
      .filter(_.id > 0)
      // compile the section stream
      .compile
      // accept sections in order
      .fold((0, Vector.empty[TypeIdx], EmptyModule)) {
        case ((idx, _, _), sec) if sec.id <= idx =>
          throw new BinaryException(s"${nameOf(sec.id)} section may not appear after ${nameOf(idx)} section")
        case ((_, tpes, mod), sec @ Section.Types(functypes)) =>
          (sec.id, tpes, mod.copy(types = functypes))
        case ((_, tpes, mod), sec @ Section.Imports(imports)) =>
          (sec.id, tpes, mod.copy(imports = imports))
        case ((_, _, mod), sec @ Section.Functions(tpes)) =>
          (sec.id, tpes, mod)
        case ((_, tpes, mod), sec @ Section.Tables(tables)) =>
          (sec.id, tpes, mod.copy(tables = tables))
        case ((_, tpes, mod), sec @ Section.Memories(mems)) =>
          (sec.id, tpes, mod.copy(mems = mems))
        case ((_, tpes, mod), sec @ Section.Globals(globals)) =>
          (sec.id, tpes, mod.copy(globals = globals))
        case ((_, tpes, mod), sec @ Section.Exports(exports)) =>
          (sec.id, tpes, mod.copy(exports = exports))
        case ((_, tpes, mod), sec @ Section.Start(start)) =>
          (sec.id, tpes, mod.copy(start = Some(start)))
        case ((_, tpes, mod), sec @ Section.Elements(elem)) =>
          (sec.id, tpes, mod.copy(elem = elem))
        case ((_, tpes, mod), sec @ Section.Code(code)) =>
          if (code.size != tpes.size)
            throw new BinaryException("code and function sections must have the same number of elements")
          val funcs = code.zip(tpes).map {
            case (FuncBody(locals, code), typeIdx) =>
              val locs = locals.flatMap {
                case LocalEntry(count, tpe) => Vector.fill(count)(tpe)
              }
              Func(typeIdx, locs, code)
          }
          (sec.id, Vector.empty, mod.copy(funcs = funcs))
        case ((_, tpes, mod), sec @ Section.Datas(data)) =>
          (sec.id, tpes, mod.copy(data = data))
      }
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
