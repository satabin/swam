package swam
package optin
package coverage

import java.nio.file.Paths

import cats.effect.{Blocker, IO}
import swam.runtime.{Engine, Instance}
import swam.text.Compiler
import utest.{TestSuite, Tests, assert}

import scala.collection.mutable.ListBuffer

/**
  *@author Javier Cabrera-Arteaga on 2020-06-11
  */
object CoverageTests extends TestSuite {

  def runCoverage(wasmFile: String): Instance[IO] = {
    implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

    val instance: Instance[IO] =
      Blocker[IO]
        .use { blocker =>
          for {
            engine <- Engine[IO](blocker)
            tcompiler <- Compiler[IO](blocker)
            m <- engine.compile(tcompiler.stream(Paths.get(wasmFile), true, blocker))
            i <- m.instantiate
          } yield i
        }
        .unsafeRunSync()
    instance
  }

  def test1(wasmFile: String) = {

    val instance = runCoverage(wasmFile)
    val list: ListBuffer[ModuleCoverageInfo] = CoverageType.buildCoverage(instance)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      assert(m == "add", c == 0, t == 4)
    }
  }

  def test2(wasmFile: String) = {

    val instance = runCoverage(wasmFile)
    val add = instance.exports.typed.function[(Int, Int), Int]("add").unsafeRunSync()
    add(1, 2).unsafeRunSync()
    val list: ListBuffer[ModuleCoverageInfo] = CoverageType.buildCoverage(instance)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      assert(m == "add", c == 4, t == 4)
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
