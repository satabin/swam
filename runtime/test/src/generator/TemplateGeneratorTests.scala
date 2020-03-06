package swam
package import_generator

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Paths
import java.util.concurrent.Executors

import cats.effect.{Blocker, IO}
import pureconfig.ConfigSource
import swam.import_generator.TemplateEngine
import swam.runtime.Engine
import swam.runtime.config.EngineConfiguration
import swam.runtime.import_generator.ImportGenerator
import swam.runtime.imports.{AsInstance, AsInterface, TCMap}
import swam.syntax.Section.Imports
import swam.text.Compiler
import swam.validation.Validator
import utest.{TestSuite, Tests, test}

import scala.concurrent.ExecutionContext

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  */
object TemplateGeneratorTests extends TestSuite {

  val engine = Engine[IO]().unsafeRunSync() // Max memory 2.0Gi

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  val blockingPool = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  val blocker: Blocker = Blocker.liftExecutionContext(blockingPool)

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  def buffer = {
    val buffer = ByteBuffer.allocate(2 * 64 * 1000)
    buffer.limit(64 * 1000)
    buffer
  }

  override def tests: Tests = Tests {

    def testImportGenerationForProgram()(implicit rootPath: utest.framework.TestPath): Unit = {

      val generator = ImportGenerator.make()

      val module =
        engine
          .compileBytes(fs2.io.file.readAll[IO](Paths.get(rootPath.value.last), blocker, 4096))
          .unsafeRunSync()

      val text = generator.generateImportText(module.imports)

      println(text)
    }

    test("templateRenderer") {
      val context = Map("F" -> "IO")

      println(TemplateEngine().render("ImportTemplate.sc", context))
    }

    test("runtime/test/resources/program.wasm") { testImportGenerationForProgram() }
  }
}
