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
package parser

import unresolved._

import fastparse._
import WastWhitespace._

object TestScriptParser {

  import Lexical._
  import Instructions._

  def module[_: P]: P[TestModule] =
    P(
      NoCut(
        ("(" ~ Index ~ word("module") ~ (id.? ~ ((word("binary") ~/ bstring.rep
          .map(ss => (idx: Int, id: Option[String]) => BinaryModule(id, ss.flatten.toArray)(idx))) | (word("quote") ~/ string.rep
          .map(ss => (idx: Int, id: Option[String]) => QuotedModule(id, ss.mkString("", "", ""))(idx))))))
          .map { case (idx, (id, f)) => f(idx, id) } ~ ")")
        | ModuleParsers.module.map(ValidModule(_)))

  def register[_: P]: P[Register] =
    P("(" ~ Index ~ word("register") ~/ string ~ id.? ~ ")").map {
      case (pos, name, id) => Register(name, id)(pos)
    }

  def action[_: P]: P[Action] =
    P("(" ~ Index ~ ((word("invoke") ~/ id.? ~ string ~ expr).map {
      case (id, name, params) => Invoke(id, name, params) _
    }
      | (word("get") ~/ id.? ~ string).map {
        case (id, name) => Get(id, name) _
      }) ~ ")").map { case (idx, f) => f(idx) }

  def assertion[_: P]: P[Assertion] =
    P(
      "(" ~ Index ~ ((word("assert_return") ~/ action ~ result).map {
        case (action, Left(result)) => AssertReturn(action, result) _
        case (action, Right(true))  => AssertReturnCanonicalNaN(action) _
        case (action, Right(false)) => AssertReturnArithmeticNaN(action) _
      }
        | (word("assert_trap") ~/ ((action ~ string)
          .map {
            case (action, msg) => AssertTrap(action, msg) _
          }
          | (module ~ string).map {
            case (module, msg) => AssertModuleTrap(module, msg) _
          }))
        | (word("assert_malformed") ~/ module ~ string)
          .map {
            case (module, msg) => AssertMalformed(module, msg) _
          }
        | (word("assert_invalid") ~/ module ~ string).map {
          case (module, msg) => AssertInvalid(module, msg) _
        }
        | (word("assert_unlinkable") ~/ module ~ string)
          .map {
            case (module, msg) => AssertUnlinkable(module, msg) _
          }
        | (word("assert_exhaustion") ~/ action ~ string)
          .map {
            case (action, msg) => AssertExhaustion(action, msg) _
          }) ~ ")").map { case (idx, f) => f(idx) }

  def result[_: P]: P[Either[Expr, Boolean]] =
    P(
      ("(" ~ (word("f32.const") | word("f64.const")) ~ (word("nan:canonical").map(_ => Right(true)) | word(
        "nan:arithmetic")
        .map(_ => Right(false))) ~ ")") | expr.map(Left(_)))

  def meta[_: P]: P[Meta] =
    P("(" ~ Index ~ ((word("script") ~/ id.? ~ script).map {
      case (id, sc) => Script(id, sc) _
    }
      | (word("input") ~/ id.? ~ string).map {
        case (id, f) => Input(id, f) _
      }
      | (word("output") ~/ id.? ~ string.?).map {
        case (id, f) => Output(id, f) _
      }) ~ ")").map { case (idx, f) => f(idx) }

  def command[_: P]: P[Command] =
    P(NoCut(register) | NoCut(action) | NoCut(assertion) | NoCut(meta) | module)

  def script[_: P]: P[Seq[Command]] =
    P(ws ~ command.rep ~ ws ~ End)

}
