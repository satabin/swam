package swam
package code_analysis
package coverage

import java.nio.file.{Paths, Path}

import cats.effect.{Blocker, IO}
import swam.runtime.{Engine, Instance, Value, Module}
import swam.text.Compiler
import utest._

import scala.collection.mutable.ListBuffer

import runtime.wasi._
import io.odin._
import cats.effect.{ContextShift, IO}
import cats.effect.Timer

/**
  *@author Javier Cabrera-Arteaga on 2020-06-11
  */
object CoverageTests extends TestSuite {

  implicit val contextShiftIO: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)

  implicit val timer: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.global)

  def doRunCovWasi(module: Module[IO],
                   main: String,
                   preopenedDirs: List[Path],
                   arguments: Vector[Value],
                   args: List[String],
                   wasi: Boolean,
                   blocker: Blocker) = {
    val logger = consoleLogger[IO]()

    if (wasi)
      Wasi[IO](preopenedDirs, args, logger, blocker).use { wasi =>
        for {
          instance <- module.importing("wasi_snapshot_preview1", wasi).instantiate
          main <- instance.exports.function(main)
          memory <- instance.exports.memory("memory")
          _ <- wasi.mem.complete(memory)
          _ <- main.invoke(arguments, Option(memory))
        } yield instance
      }
    else
      for {
        instance <- module.instantiate
        main <- instance.exports.function(main)
        r <- main.invoke(arguments, None)
      } yield instance
  }

  def runCoverage(wasmFile: String,
                  main: String,
                  args: List[String],
                  vec: Vector[Value],
                  wat: Boolean,
                  wasi: Boolean): CoverageListener[IO] = {

    val coverageListener = CoverageListener[IO](wasi, ".", false, false)

    Blocker[IO]
      .use { blocker =>
        for {
          engine <- Engine[IO](blocker, listener = Option(coverageListener))
          tcompiler <- Compiler[IO](blocker)

          m <- if (wat) engine.compile(tcompiler.stream(Paths.get(wasmFile), true, blocker))
          else engine.compile(Paths.get(wasmFile), blocker)
          _ <- doRunCovWasi(m, main, List(), vec, List(), wasi, blocker)
        } yield {}
      }
      .unsafeRunSync()

    coverageListener
  }

  def test1(wasmFile: String) = {

    val vec = Vector(Value.Int32(10), Value.Int32(10))

    val listener = runCoverage(wasmFile, "add", List(), vec, true, false)

    val list = CoverageReporter.buildCoverage(listener)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      if (m == "add")
        assert(c == 4, t == 4) // 100 % percent covered
    }
  }

  def test2(wasmFile: String) = {

    val vec = Vector()
    val listener = runCoverage(wasmFile, "_start", List(), vec, false, true)

    val list = CoverageReporter.buildCoverage(listener)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      if (m == "__original_main")
        assert(c == 61, t == 74) // 82% percent covered
    }
  }

  def test3(wasmFile: String) = {

    val vec = Vector()
    val listener = runCoverage(wasmFile, "_start", List(), vec, false, true)

    val list = CoverageReporter.buildCoverage(listener)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      if (m == "__original_main")
        assert(c == 75, t == 89) // 84 % percent covered
    }
  }

  def test4(wasmFile: String) = {

    val vec = Vector()
    val listener = runCoverage(wasmFile, "_start", List(), vec, false, true)

    val list = CoverageReporter.buildCoverage(listener)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      if (m == "__original_main")
        assert(c == 75, t == 75) // 100 % percent covered
    }
  }

  def test5(wasmFile: String) = {

    val vec = Vector()
    val listener = runCoverage(wasmFile, "_start", List(), vec, false, true)

    val list = CoverageReporter.buildCoverage(listener)

    for (l <- list) {
      val ModuleCoverageInfo(m, c, t) = l
      if (m == "__original_main")
        assert(c == 514, t == 514) // 100 % percent covered
      else if (m == "deconv")
        assert(c == 203, t == 214) // 94 % percent covered
      else if (m == "pad_two")
        assert(c == 0, t == 47) // 0 % percent covered
      else if (m == "fft")
        assert(c == 53, t == 53) // 100 % percent covered
      else if (m == "_fft")
        assert(c == 192, t == 229) // 83 % percent covered
    }
  }

  val tests = Tests {

    /**
    TODO more manual test cases to be added.
      */
    "add" - test1("code_analysis/test/resources/coverage-test/add.wat")
    "check-if" - test2("code_analysis/test/resources/coverage-test/informal_data/wasm_programs/if_else-check-if.wasm")
    "check-else" - test3(
      "code_analysis/test/resources/coverage-test/informal_data/wasm_programs/if_else-check-else.wasm")
    "check-for" - test4("code_analysis/test/resources/coverage-test/informal_data/wasm_programs/check-for.wasm")
    "deconvolution" - test5("code_analysis/test/resources/coverage-test/formal_data/Deconvolution-1D.wasm")
  }
}
