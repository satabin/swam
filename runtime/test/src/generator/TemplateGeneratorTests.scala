package swam
package import_generator

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Paths
import java.util.concurrent.Executors

import cats.effect.{Async, Blocker, IO}
import pureconfig.ConfigSource
import swam.runtime.{Engine, formats, imports}
import swam.runtime.config.EngineConfiguration
import swam.runtime.formats.DefaultFormatters
import swam.runtime.import_generator.ImportGenerator
import swam.runtime.imports.{AsInstance, AsInterface, TCMap}
import swam.syntax.Section.Imports
import swam.text.Compiler
import swam.validation.Validator
import utest.{TestSuite, Tests, test}

import scala.concurrent.ExecutionContext
import scala.reflect.runtime._

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  */
object TemplateGeneratorTests extends TestSuite {

  val engine = Engine[IO]().unsafeRunSync() // Max memory 2.0Gi

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  val blockingPool = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  val blocker: Blocker = Blocker.liftExecutionContext(blockingPool)

  override def tests: Tests = Tests {
    val f1 = "runtime/test/resources/program3.wasm"
    val f2 = "runtime/test/resources/program.wasm"
    val generator = ImportGenerator.make()

    test("generation") {

      val modul1 =
        engine
          .compileBytes(fs2.io.file.readAll[IO](Paths.get(f1), blocker, 4096))
          .unsafeRunSync()

      val modul2 =
        engine
          .compileBytes(fs2.io.file.readAll[IO](Paths.get(f2), blocker, 4096))
          .unsafeRunSync()

      val text = generator.generateImportText(modul1.imports.concat(modul2.imports))

      println(text)
    }

    test("create project") {

      val modul1 =
        engine
          .compileBytes(fs2.io.file.readAll[IO](Paths.get(f1), blocker, 4096))
          .unsafeRunSync()

      val modul2 =
        engine
          .compileBytes(fs2.io.file.readAll[IO](Paths.get(f2), blocker, 4096))
          .unsafeRunSync()

      generator.createScalaProjectForImports("test_scala", modul1.imports)
    }
  }
}
