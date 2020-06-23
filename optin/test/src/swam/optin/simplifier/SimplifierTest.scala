package swam
package optin
package simplifier

import java.nio.file.Paths

import cats.effect.{Blocker, IO}
import swam.optin.coverage.CoverageListener
import swam.optin.verifier.PathExtractor
import swam.runtime.internals.instance.FunctionInstance
import swam.runtime.{Engine, Instance, Value}
import swam.text.Compiler
import utest._

import scala.collection.mutable.ListBuffer

/**
  *@author Javier Cabrera-Arteaga on 2020-06-11
  */
object SimplifierTest extends TestSuite {

  def runCoverage(wasmFile: String, main: String): Any = {

    implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)
    Blocker[IO]
      .use { blocker =>
        for {
          engine <- Engine[IO](blocker)
          extractor = PathExtractor[IO](engine.asm)
          tcompiler <- Compiler[IO](blocker)
          m <- engine.compile(tcompiler.stream(Paths.get(wasmFile), true, blocker))
          i <- m.instantiate
          f <- i.exports.function(main) // Calling function
          _ <- IO(extractor.symbolicEval(f.asInstanceOf[FunctionInstance[IO]]))
        } yield {}
      }
      .unsafeRunSync()
    None
  }

  def test1(wasmFile: String) = {

    runCoverage(wasmFile, "add")

  }

  val tests = Tests {

    /**
        TODO more manual test cases to be added.
      */
    //"inst1" - runCoverage("runtime/test/resources/coverage-test/1_inst.wasm")
    "add" - test1("optin/test/resources/coverage-test/add.wat")
    "add2" - test1("optin/test/resources/coverage-test/add2.wat")
    "add3" - test1("optin/test/resources/coverage-test/add3.wat")
  }
}
