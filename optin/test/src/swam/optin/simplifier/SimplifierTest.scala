package swam
package optin
package simplifier

import java.io.File
import java.nio.file.Paths

import cats.effect.{Blocker, IO}
import swam.optin.coverage.CoverageListener
import swam.optin.verifier.PathExtractor
import swam.runtime.internals.instance.FunctionInstance
import swam.runtime.{Engine, Instance, Value}
import swam.text.Compiler
import utest._

import sys.process._
import scala.language.postfixOps
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
          graph <- IO(extractor.symbolicEval(f.asInstanceOf[FunctionInstance[IO]]))
          _ <- IO(println((s"echo '$graph'" #> new File(s"$wasmFile.dot")).!))
          _ <- IO(println((s"dot -Tpng $wasmFile.dot -o $wasmFile.png").!))
        } yield {}
      }
      .unsafeRunSync()
    None
  }

  def test1(wasmFile: String, main: String = "add") = {

    runCoverage(wasmFile, main)

  }

  val tests = Tests {

    /**
        TODO more manual test cases to be added.
      */
    //"inst1" - runCoverage("runtime/test/resources/coverage-test/1_inst.wasm")
    "add" - test1("optin/test/resources/coverage-test/add.wat")
    "add2" - test1("optin/test/resources/coverage-test/add2.wat")
    "add3" - test1("optin/test/resources/coverage-test/add3.wat")
    "multi" - test1("optin/test/resources/coverage-test/if-nested.wat", "nested")
  }
}
