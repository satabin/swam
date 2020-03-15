package swam
package interpreter

import java.io.File
import java.nio.ByteBuffer
import java.util.logging.{Formatter, LogRecord}

import swam._
import text._
import runtime._
import runtime.trace._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import swam.runtime.Engine
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.formats.DefaultFormatters._

/**
    @author Javier Cabrera-Arteaga on 2020-03-12
  */
case class Config(wasm: File = null,
                  main: String = "",
                  parse: Boolean = false,
                  debugCompiler: Boolean = false,
                  trace: Boolean = false,
                  traceOutDir: String = ".",
                  traceFilter: String = "*",
                  tracePattern: String = "log.%u.txt")

private object NoTimestampFormatter extends Formatter {
  override def format(x: LogRecord): String =
    x.getMessage
}

object InterpreterApp extends IOApp {

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  val parser = new scopt.OptionParser[Config]("swam-interpreter") {
    head("swam-interpreter")

    help("help").text("prints this usage text")
    note("swam-interpreter executes a WASM binary")

    arg[File]("<wasm>")
      .required()
      .action((x, c) => c.copy(wasm = x))
      .text("WASM module to be executed")

    opt[String]('m', "main")
      .optional()
      .action((f, c) => c.copy(main = f))
      .text("Executes provided function name as main. Prints available functions if no name is provided")

    opt[Boolean]('p', "parse")
      .optional()
      .action((f, c) => c.copy(parse = f))
      .text("Parse the input file as text")

    opt[String]('f', "filter")
      .optional()
      .action((f, c) => c.copy(traceFilter = f))
      .text("Filter the traces. The parameter is a regular expression applied to the opcode, e.g.: 'mread|mwrite'")

    opt[String]('l', "logfile-name")
      .optional()
      .action((f, c) => c.copy(tracePattern = f))
      .text("SWAM uses a logger based on [[https://docs.oracle.com/en/java/javase/13/docs/api/java.logging/java/util/logging/package-summary.html java.util.logging]]. Provide any valid valid file pattern")

    opt[Boolean]('d', "debug-compiler")
      .optional()
      .action((f, c) => c.copy(debugCompiler = f))
      .text("Activates the debug option for thee text parser")

    opt[Boolean]('t', "trace")
      .optional()
      .action((f, c) => c.copy(trace = f))
      .text("Traces WASM execution channels; stack, memory and opcodes")

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

  def createInstance(blocker: Blocker, config: Config): IO[Instance[IO]] = {
    for {
      compiler <- Compiler[IO]
      engine <- if (config.trace)
        Engine[IO](
          tracer = Option(JULTracer(config.traceOutDir, config.tracePattern, NoTimestampFormatter, config.traceFilter)))
      else
        Engine[IO]()

      module <- if (config.parse)
        engine.compile(compiler.stream(config.wasm.toPath, config.debugCompiler, blocker))
      else
        engine.compile(config.wasm.toPath, blocker, 4096)

      instance <- engine.instantiate(module, imports())
    } yield instance
  }

  def run(args: List[String]): IO[ExitCode] = parser.parse(args, Config()) match {
    case Some(config) =>
      Blocker[IO].use { blocker =>
        for {
          instance <- createInstance(blocker, config)
          _ <- if (config.main.isEmpty) {
            IO(println("Listing available functions..."))
            instance.exports.list
              .collect[String, FuncType] {
                case (name, f: FuncType) =>
                  (name, f)
              }
              .foreach {
                case (name, tpe: FuncType) => {
                  println(s"\t ${Console.GREEN} $name ${Console.RESET}${tpe.params.mkString("( ", ",", " )")} -> ${tpe.t
                    .mkString(",")}")
                }
              }
            IO()
          } else {
            for {
              f <- instance.exports.function(config.main)
              _ <- f.invoke(Vector(), None)
            } yield IO(ExitCode.Success)
          }
        } yield ()

        IO(ExitCode.Success)
      }
    case None => {
      parser.reportError("You must provide a WASM file")
      IO(ExitCode.Error)
    }
  }
}
