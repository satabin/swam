package swam
package interpreter

import java.io.{File, PrintWriter}
import java.nio.ByteBuffer
import java.util.logging.{Formatter, LogRecord}

import swam.{wasi, _}
import text._
import runtime._
import runtime.trace._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import swam.runtime.Engine
import swam.runtime.Value.Int32
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
                  traceWasi: Boolean = false,
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

    opt[Boolean]('r', "trace-wasi")
      .optional()
      .action((f, c) => c.copy(traceWasi = f))
      .text("Traces WASM execution channels; stack, memory and opcodes including wasu")

    opt[File]('l', "link")
      .optional()
      .action((f, c) => c.copy(linkJarPath = f))
      .text("Links the imports inside the jar file to be append in the executed WASM")

    opt[String]('c', "class-name")
      .optional()
      .action((f, c) => c.copy(className = f))
      .text("Imports class name")

  }

  def getMemory(mem: Memory[IO], traceWasi: Boolean): Memory[IO] = {
    mem match {
      case m: MemoryInstance[IO] => m
      case m: TracingMemory[IO]  => if (traceWasi) m else m.inner
    }
  }

  def prepareWASI(module: Module[IO], config: Config) =
    for {
      wasi <- IO(new wasi.WASIImplementation(args = config.args.toSeq))
      instance <- module.importing(wasi.imports()).instantiate
      mem <- instance.exports.memory("memory")
      wrappMem <- IO(getMemory(mem, config.traceWasi))
      _ <- IO(wasi.mem = wrappMem)
    } yield (instance, mem, wrappMem)

  def prepeareNonWASI(module: Module[IO], config: Config) =
    for {
      instance <- module.instantiate
      mem <- instance.exports.memory("memory")
      wrappMem <- IO(getMemory(mem, config.traceWasi))
    } yield (instance, mem, wrappMem)

  def createInstance(blocker: Blocker, config: Config) = {
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

      (instance, mem, wrappMem) <- if (module.imports.collect { case x => x.moduleName.startsWith("wasi_") }.nonEmpty)
        prepareWASI(module, config)
      else
        prepeareNonWASI(module, config)
      (length, offset) <- IO(writeArgsToMemory(wrappMem, config.wasm.getName +: config.args))
    } yield (instance, wrappMem, length, offset)
  }

  def writeSimpleArg(mem: Memory[IO], arg: String, offset: Int, ptrOffset: Int) = {

    mem.writeInt(ptrOffset, offset).unsafeRunSync()
    mem.writeBytes(offset, ByteBuffer.wrap(arg.getBytes())).unsafeRunSync()

    (ptrOffset + 4, offset + arg.getBytes.length + 1)
  }

  def writeArgsToMemory(mem: Memory[IO], args: Vector[String]) = {
    val lastPosition = mem.size

    // create size
    val newSize = 4 * args.length + args.map(a => a.getBytes().length + 1).sum // trailing zero

    val scale = Math.ceil((newSize + mem.size) / mem.size).toInt
    // grow a little bit
    mem.grow(scale)

    val headSize = 4 * args.length

    var prev = lastPosition + headSize
    //write_args_so_far
    args.zipWithIndex.foreach {
      case (arg, index) => {
        val ptrs = writeSimpleArg(mem, arg, prev, lastPosition + index * 4)
        prev = ptrs._2
      }
    }

    def printMem() = {
      val f = new PrintWriter(new File("trace.mem"))

      for (i <- 0 until mem.size)
        f.write(s"${(mem.readByte(i).unsafeRunSync().toChar)}")

      f.close()
    }

    /// printMem()

    (args.length, lastPosition)
  }

  def getMainFunctionArgs(argc: Int, args: Int) = if (argc <= 2) Vector() else Vector(Int32(argc), Int32(args))

  def run(args: List[String]): IO[ExitCode] = parser.parse(args, Config()) match {
    case Some(config) =>
      Blocker[IO].use { blocker =>
        for {
          (instance, mem, argC, args) <- createInstance(blocker, config)
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
              _ <- f.invoke(getMainFunctionArgs(argC, args), Option(mem))
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
