package swam
package optin
package coverage

import java.nio.file.Paths

import cats.effect.{Blocker, IO}
import swam.runtime.{Engine, Instance, Value}
import swam.text.Compiler
import utest._

import scala.collection.mutable.ListBuffer

/**
  *@author Javier Cabrera-Arteaga on 2020-06-11
  */
object CoverageTests extends TestSuite {

  def runCoverage(wasmFile: String, main: String): CoverageListener[IO] = {
    implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)
    val coverageListener = CoverageListener[IO]()

    Blocker[IO]
      .use { blocker =>
        for {
          engine <- Engine[IO](blocker, listener = Option(coverageListener))
          tcompiler <- Compiler[IO](blocker)
          m <- engine.compile(tcompiler.stream(Paths.get(wasmFile), true, blocker))
          i <- m.instantiate
          f <- i.exports.function(main) // Calling function
          _ <- f.invoke(Vector(Value.Int32(10), Value.Int32(10)), None)
        } yield {}
      }
      .unsafeRunSync()
    coverageListener
  }

  def test1(wasmFile: String) = {

    val listener = runCoverage(wasmFile, "add")

    val list = CoverageReporter.buildCoverage(listener)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      if (m == "add")
        assert(c == 4, t == 4) // 100 % percent covered
    }
  }

  val tests = Tests {

    /**
    TODO more manual test cases to be added.
      */
    //"inst1" - runCoverage("runtime/test/resources/coverage-test/1_inst.wasm")
    "add" - test1("optin/test/resources/coverage-test/add.wat")
  }
}
