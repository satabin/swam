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

import parser._
import runtime._

import swam.test.util._

import utest._

import better.files._

import fastparse._

import cats.effect._

import scala.concurrent.ExecutionContext

object SpecTests extends TestSuite {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def run(wast: File) = {
    val positioner = new WastPositioner(wast.path)
    val script = parse(wast.contentAsString, TestScriptParser.script(_)).get.value
    val engine = new ScriptEngine
    engine.run(script, positioner).unsafeRunSync()
  }

  val tests = testfiles("runtime/test/resources/spec-test", run _)

}
