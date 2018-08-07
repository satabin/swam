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

import fastparse.noApi._

object TestScriptParser {

  import Lexical._
  import white._
  import Instructions._

  val module: P[TestModule] =
    P(
      NoCut(
        ("(" ~ word("module") ~ (id.!.? ~ ((word("binary") ~/ string.rep
          .map(ss => (id: Option[String]) => BinaryModule(id, ss.mkString("", "", "")))) | (word("quote") ~/ string.rep
          .map(ss => (id: Option[String]) => QuotedModule(id, ss.mkString("", "", ""))))))
          .map { case (id, f) => f(id) }) ~ ")")
        | ModuleParsers.module.map(ValidModule(_)))

  val register: P[Register] =
    P("(" ~ word("register") ~/ string ~ id.!.? ~ ")").map(Register.tupled)

  val action: P[Action] =
    P(
      "(" ~ ((word("invoke") ~/ id.!.? ~ string ~ expr).map(Invoke.tupled)
        | (word("get") ~/ id.!.? ~ string).map(Get.tupled)) ~ ")")

  val assertion: P[Assertion] =
    P(
      "(" ~ ((word("assert_return") ~/ action ~ expr).map(AssertReturn.tupled)
        | (word("assert_return_canonical_nan") ~/ action)
          .map(AssertReturnCanonicalNaN)
        | (word("assert_return_arithmetic_nan") ~/ action)
          .map(AssertReturnArithmeticNaN)
        | (word("assert_trap") ~/ ((action ~ string)
          .map(AssertTrap.tupled) | (module ~ string).map(AssertModuleTrap.tupled)))
        | (word("assert_malformed") ~/ module ~ string)
          .map(AssertMalformed.tupled)
        | (word("assert_invalid") ~/ module ~ string).map(AssertInvalid.tupled)
        | (word("assert_unlinkable") ~/ module ~ string)
          .map(AssertUnlinkable.tupled)
        | (word("assert_exhaustion") ~/ action ~ string)
          .map(AssertExhaustion.tupled)) ~ ")")

  val meta: P[Meta] =
    P(
      "(" ~ ((word("script") ~/ id.!.? ~ script).map(Script.tupled)
        | (word("input") ~/ id.!.? ~ string).map(Input.tupled)
        | (word("output") ~/ id.!.? ~ string.?).map(Output.tupled)) ~ ")")

  val command: P[Command] =
    P(register | action | assertion | meta | module)

  val script: P[Seq[Command]] =
    P(ws ~ command.rep ~ ws ~ End)

}
