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
import swam.runtime.internals.instance.MemoryInstance

/**
    @author Javier Cabrera-Arteaga on 2020-03-12
  */
case class Config(wasm: File = null,
                  args: Vector[String] = Vector(),
                  main: String = "",
                  parse: Boolean = false,
                  debugCompiler: Boolean = false,
                  trace: Boolean = false,
                  traceOutDir: String = ".",
                  traceFilter: String = "*",
                  tracePattern: String = "log.txt",
                  linkJarPath: File = null,
                  className: String = "")

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

    arg[String]("<args>...")
      .unbounded()
      .required()
      .minOccurs(0)
      .action((x, c) => c.copy(args = c.args :+ x))
      .text("Input arguments")

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
      .text("Activates the debug option for the text parser")

    opt[Boolean]('t', "trace")
      .optional()
      .action((f, c) => c.copy(trace = f))
      .text("Traces WASM execution channels; stack, memory and opcodes")

    opt[File]('l', "link")
      .optional()
      .action((f, c) => c.copy(linkJarPath = f))
      .text("Links the imports inside the jar file to be append in the executed WASM")

    opt[String]('c', "class-name")
      .optional()
      .action((f, c) => c.copy(className = f))
      .text("Imports class name")

  }

  def getMemoryBuffer(mem: Memory[IO]): ByteBuffer = {
    mem match {
      case m: MemoryInstance[IO] => m.buffer
      case m: TracingMemory[IO]  => getMemoryBuffer(m.inner)
    }
  }

  def createInstance(blocker: Blocker, config: Config): IO[Instance[IO]] = {
    for {
      imports <- IO(JVMLinker().getImports(config.linkJarPath, config.className))
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

      wasi <- IO(new wasi.WASIImplementation(args = config.args.toSeq))
      instance <- engine.instantiate(module, wasi.imports())
      mem <- instance.exports.memory("memory")
      _ <- IO(wasi.mem = getMemoryBuffer(mem))
    } yield instance
  }

  def run(args: List[String]): IO[ExitCode] = parser.parse(args, Config()) match {
    case Some(config) =>
      Blocker[IO].use { blocker =>
        for {
          instance <- createInstance(blocker, config)
          _ <- if (config.main.isEmpty) {
            IO(instance.exports.list
              .collect[String, FuncType] {
                case (name, f: FuncType) =>
                  (name, f)
              }
              .foreach {
                case (name, tpe: FuncType) =>
                  println(s"\t ${Console.GREEN} $name ${Console.RESET}${tpe.params.mkString("( ", ",", " )")} -> ${tpe.t
                    .mkString(",")}")

              })
          } else {
            for {
              f <- instance.exports.function(config.main)
              _ <- f.invoke(Vector(), None)
            } yield ()
          }
        } yield ExitCode.Success
      }
    case None => {
      parser.reportError("You must provide a WASM file")
      IO(ExitCode.Error)
    }
  }
}
