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
package runtime
package coverage

import swam._
import text._
import runtime._
import runtime.coverage._
import cats.effect._

import utest._

import java.nio.file.Paths

import scala.collection.mutable.ListBuffer

object CoverageTests extends TestSuite {

  def runCoverage(wasmFile: String) : Instance[IO]= {
    implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

    val instance: Instance[IO] =
      Blocker[IO].use { blocker =>
        for {
          engine <- Engine[IO](blocker)
          tcompiler <- Compiler[IO](blocker)
          m <- engine.compile(tcompiler.stream(Paths.get(wasmFile), true, blocker))
          i <- m.instantiate
        } yield i
      }.unsafeRunSync()
    instance
  }


  def test1(wasmFile: String) = {
      
      val instance = runCoverage(wasmFile)
      val list : ListBuffer[ModuleCoverageInfo] = CoverageType.buildCoverage(instance)

      for (l <- list) {
        val ModuleCoverageInfo(m, c, t) = l
        assert(m == "add", c==0, t==4)
      }
  }

  def test2(wasmFile: String) = {

      val instance = runCoverage(wasmFile)
      val add = instance.exports.typed.function[(Int, Int), Int]("add").unsafeRunSync()
      add(1,2).unsafeRunSync()
      val list : ListBuffer[ModuleCoverageInfo] = CoverageType.buildCoverage(instance)
      
      for (l <- list) {
        val ModuleCoverageInfo(m, c, t) = l
        assert(m == "add", c==4, t==4)
      }
      
  }

  val tests = Tests {
      /**
      TODO more manual test cases to be added.
      */
      //"inst1" - runCoverage("runtime/test/resources/coverage-test/1_inst.wasm")
     "add" - test1("runtime/test/resources/coverage-test/add.wat")
     "addWithCov" - test2("runtime/test/resources/coverage-test/add.wat")
  }
}
