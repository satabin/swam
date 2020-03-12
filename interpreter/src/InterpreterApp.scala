package swam
package interpreter

import java.io.File
import java.nio.ByteBuffer

import swam.runtime._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import swam.runtime.Engine
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}

/**
    @author Javier Cabrera-Arteaga on 2020-03-12
  */
case class Config(wasm: File = null, main: String = "")

object InterpreterApp extends IOApp {

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("swam-generator")

    help("help").text("prints this usage text")
    note("swam-interpreter executes a WASM binary")

    opt[String]('s', "function")
      .optional()
      .action((f, c) => c.copy(main = f))
      .text("Executes provided function name as main. Executes the start function instead")

    arg[File]("<wasm>")
      .required()
      .action((x, c) => c.copy(wasm = x))
      .text("WASM module to be executed")

  }

  def buffer = {
    val buffer = ByteBuffer.allocate(2 * pageSize)
    buffer.limit(pageSize)
    buffer
  }

  def imports() =
    Imports[IO](
      TCMap[String, AsIsIO](
        "env" -> TCMap[String, AsIIO]("memory" -> buffer)
      ))

  def run(args: List[String]): IO[ExitCode] = parser.parse(args, Config()) match {
    case Some(config) =>
      Blocker[IO].use { blocker =>
        for {

          engine <- Engine[IO]()
          module <- engine.compile(config.wasm.toPath, blocker, 4096)
          instance <- engine.instantiate(module, imports())
          _ <- instance.exports.function("main")
          _ <- IO(println(123))
          exit <- IO(ExitCode.Success)
        } yield (exit)
      }
    case None => {
      parser.reportError("You must provide a WASM file")
      IO(ExitCode.Error)
    }
  }
}
