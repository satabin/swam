package swam
package cli

import java.nio.file.{Path, Paths, StandardOpenOption}
import java.util.concurrent.TimeUnit
import java.util.logging.{LogRecord, Formatter => JFormatter}

import cats.effect.{Blocker, Clock, ExitCode, IO}
import cats.implicits._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import io.odin.formatter.Formatter
import io.odin.formatter.options.ThrowableFormat
import io.odin.{Logger, consoleLogger}
import fs2._
import swam.ValType.{F32, F64, I32, I64}
import swam.binary.ModuleStream
import swam.decompilation._
import swam.code_analysis.coverage.{CoverageListener, CoverageReporter}
import swam.runtime.imports._
import swam.runtime.trace._
import swam.runtime.wasi.Wasi
import swam.runtime.{Engine, Function, Module, Value}
import swam.text.Compiler
import swam.binary.custom.{FunctionNames, ModuleName}
import swam.runtime.internals.compiler.CompiledFunction

private object NoTimestampFormatter extends JFormatter {
  override def format(x: LogRecord): String =
    x.getMessage
}

object Main extends CommandIOApp(name = "swam-cli", header = "Swam from the command line") {

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  ////// CLI-COMMAND ARGUMENTS //////
  // Beware (Opts.argument != Opts.arguments) && (Opts.option != Opts.options) && (Opts.flag != Opts.flags)

  val wasmFile =
    Opts.argument[Path](metavar = "wasm")

  val func_name =
    Opts.argument[String](metavar = "functionName")

  // Arguments that get passed to the WASM code you execute. They are available through WASI args_get.
  val restArguments =
    Opts.arguments[String](metavar = "args").orEmpty

  val wasmArgTypes = Opts
    .options[String]("argType",
                     help = "Input parameter types for Wasm function. Possible values: Int32, Int64, Float32, Float64.",
                     short = "aT")
    .orEmpty

  val dirs = Opts
    .options[Path]("dir", "Preopen directory", short = "D", metavar = "dir")
    .orEmpty

  val mainFun =
    Opts
      .option[String]("main", "Execute function of provided name (default is _start)", short = "m")
      .withDefault("_start")

  val wat =
    Opts.flag("wat", "Input file is in WAT format, and needs to be parsed as such (default false)", short = "w").orFalse

  val wasi =
    Opts.flag("wasi", "Program is using wasi (default false)", short = "W").orFalse

  val time =
    Opts.flag("time", "Measure execution time (default false)", short = "C").orFalse

  val noValidateBinary =
    Opts.flag("novalidate", "Do not validate WASM binary, imports for example").orFalse

  val trace =
    Opts.flag("trace", "Trace WASM execution channels (default false)", short = "t").orFalse

  val exportInstrumented =
    Opts
      .option[Path]("export-instrumented", "Compile and export the instrumented WASM binary")
      .withDefault(null)
  /*val cov =
    Opts.flag("instcov", "Run the WebAssembly module and gets coverage.", short = "v").orFalse*/

  val covfilter = Opts.flag("cov-filter", "Generate coverage with filter on Wasi Methods", short = "r").orFalse

  val covOut = Opts
    .option[Path]("covout", "Output folder for coverage reports and show-map", short = "c")
    .withDefault(Paths.get(".").toAbsolutePath.normalize)

  val filter =
    Opts
      .option[String](
        "filter",
        "Filter the traces. The parameter is a regular expression applied to the opcode, e.g.: 'mread|mwrite' (default *)",
        short = "f")
      .withDefault("*")

  val traceFile =
    Opts
      .option[Path](
        "trace-file",
        "The file to which traces are written (default trace.log)",
        short = "l"
      )
      .withDefault(Paths.get("trace.log"))

  val debug =
    Opts.flag("debug", "Generate debug elements when compiling wat format (default false)", short = "d").orFalse

  val dev =
    Opts.flag("exceptions", "Print exceptions with stacktrace (default false)", short = "X").orFalse

  val textual =
    Opts.flag("wat", "Decompile in wat format (requires module to be valid) (default false)", short = "w").orFalse

  val out = Opts
    .option[Path]("out", "Save decompiled result in the given file. Prints to stdout if not provider", short = "o")

  ////// CLI-COMMAND ARGUMENT COMBINATIONS //////

  val runOpts: Opts[Options] = Opts.subcommand("run", "Run a WebAssembly file") {
    (mainFun, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasmFile, restArguments, wasmArgTypes).mapN {
      (main, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasm, args, wasmArgTypes) =>
        Run(wasm, args, main, wat, wasi, time, trace, filter, traceFile, dirs, debug, wasmArgTypes)
    }
  }

  val covOpts: Opts[Options] = Opts.subcommand("coverage", "Run a WebAssembly file and generate coverage report") {
    (mainFun,
     wat,
     wasi,
     time,
     dirs,
     trace,
     traceFile,
     filter,
     debug,
     wasmFile,
     restArguments,
     exportInstrumented,
     covOut,
     covfilter,
     wasmArgTypes,
     noValidateBinary).mapN {
      (main,
       wat,
       wasi,
       time,
       dirs,
       trace,
       traceFile,
       filter,
       debug,
       wasm,
       args,
       exportInstrumented,
       covOut,
       covfilter,
       wasmArgTypes,
       noValidateBinary) =>
        WasmCov(wasm,
                args,
                main,
                wat,
                wasi,
                time,
                trace,
                filter,
                traceFile,
                dirs,
                debug,
                exportInstrumented,
                covOut,
                covfilter,
                wasmArgTypes,
                noValidateBinary)
    }
  }

  val serverOpts: Opts[Options] =
    Opts.subcommand("run_server", "Run a socket for a given WASM that listens to inputs") {
      // TODO: Check which ones of these are really necessary
      (mainFun,
       wat,
       wasi,
       time,
       dirs,
       trace,
       traceFile,
       filter,
       debug,
       wasmFile,
       restArguments,
       covfilter,
       wasmArgTypes)
        .mapN { (main, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasm, args, covfilter, wasmArgTypes) =>
          RunServer(wasm, args, main, wat, wasi, time, trace, filter, traceFile, dirs, debug, covfilter, wasmArgTypes)
        }
    }

  val decompileOpts: Opts[Options] = Opts.subcommand("decompile", "Decompile a wasm file") {
    (textual, wasmFile, out.orNone).mapN { (textual, wasm, out) => Decompile(wasm, textual, out) }
  }

  val inferOpts: Opts[Options] =
    Opts.subcommand("infer", "Get the parameters type for functions file in Wasm module.") {
      (wasmFile, wat, func_name).mapN { (wasm, wat, func_name) => Infer(wasm, wat, func_name) }
    }

  val validateOpts: Opts[Options] = Opts.subcommand("validate", "Validate a wasm file") {
    (wasmFile, wat, dev).mapN(Validate(_, _, _))
  }

  val compileOpts: Opts[Options] = Opts.subcommand("compile", "Compile a wat file to wasm") {
    (wasmFile, out, debug).mapN(Compile(_, _, _))
  }

  val outFileOptions = List(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

  def main: Opts[IO[ExitCode]] =
    runOpts
      .orElse(serverOpts)
      .orElse(covOpts)
      .orElse(inferOpts)
      .orElse(decompileOpts)
      .orElse(validateOpts)
      .orElse(compileOpts)
      .map { opts =>
        Blocker[IO].use { blocker =>
          opts match {
            case Run(file, args, main, wat, wasi, time, trace, filter, tracef, dirs, debug, wasmArgTypes) =>
              for {
                tracer <- if (trace)
                  JULTracer[IO](blocker,
                                traceFolder = ".",
                                traceNamePattern = tracef.toAbsolutePath().toString(),
                                filter = filter,
                                formatter = NoTimestampFormatter).map(Some(_))
                else
                  IO(None)
                argsParsed <- IO(parseWasmArgs(wasmArgTypes, args))
                engine <- Engine[IO](blocker, tracer)
                tcompiler <- Compiler[IO](blocker)
                module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
                compiled <- engine.compile(module)
                preparedFunction <- prepareFunction(compiled, main, dirs, args, wasi, blocker)
                _ <- IO(executeFunction(IO(preparedFunction), argsParsed, time))
              } yield ExitCode.Success

            // TODO: Remove this and instead to coverage flag in Run(...)
            case WasmCov(file,
                         args,
                         main,
                         wat,
                         wasi,
                         time,
                         trace,
                         filter,
                         tracef,
                         dirs,
                         debug,
                         exportInstrumented,
                         covOut,
                         covfilter,
                         wasmArgTypes,
                         noValidateBinary) =>
              for {
                tracer <- if (trace)
                  JULTracer[IO](blocker,
                                traceFolder = ".",
                                traceNamePattern = tracef.toAbsolutePath().toString(),
                                filter = filter,
                                formatter = NoTimestampFormatter).map(Some(_))
                else
                  IO(None)
                argsParsed <- IO(parseWasmArgs(wasmArgTypes, args))
                coverageListener = CoverageListener[IO](covfilter)
                engine <- Engine[IO](blocker, tracer, listener = Option(coverageListener))
                tcompiler <- swam.text.Compiler[IO](blocker)
                module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
                _ <- if (exportInstrumented != null) {
                  IO(println("Export"))

                  (Stream.emits(ModuleStream.header.toArray) ++ module
                    .through(ModuleStream.encoder.encode[IO])
                    .flatMap(bv => Stream.emits(bv.toByteArray)))
                    .through(fs2.io.file.writeAll(exportInstrumented, blocker, outFileOptions))
                    .compile
                    .drain

                } else { IO(println("Do not export")) }
                compiled <- if (noValidateBinary) engine.compileNotValidate(module)
                else engine.compile(module) // This is not needed since the validation is read from the config files
                preparedFunction <- prepareFunction(compiled, main, dirs, args, wasi, blocker)
                _ <- IO(executeFunction(IO(preparedFunction), argsParsed, time))
                _ <- IO(CoverageReporter.blockCoverage(covOut, file, coverageListener))
              } yield ExitCode.Success

            case RunServer(file,
                           args,
                           main,
                           wat,
                           wasi,
                           time,
                           trace,
                           filter,
                           tracef,
                           dirs,
                           debug,
                           covfilter,
                           wasmArgTypes) =>
              for {
                tracer <- if (trace)
                  JULTracer[IO](blocker,
                                traceFolder = ".",
                                traceNamePattern = tracef.toAbsolutePath().toString(),
                                filter = filter,
                                formatter = NoTimestampFormatter).map(Some(_))
                else
                  IO(None)
                coverageListener = CoverageListener[IO](covfilter)
                engine <- Engine[IO](blocker, tracer, listener = Option(coverageListener))
                tcompiler <- Compiler[IO](blocker)
                module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
                compiled <- engine.compile(module)
                preparedFunction <- prepareFunction(compiled, main, dirs, args, wasi, blocker)
                _ <- IO(
                  Server
                    .listen(IO(preparedFunction), wasmArgTypes, time, file, coverageListener))
              } yield ExitCode.Success

            case Decompile(file, textual, out) =>
              for {
                decompiler <- if (textual)
                  TextDecompiler[IO](blocker)
                else
                  RawDecompiler[IO]
                doc <- decompiler.decompilePath(file, blocker)
                outPipe = out.fold(fs2.io.stdout[IO](blocker))(fs2.io.file.writeAll[IO](_, blocker, outFileOptions))
                _ <- Stream
                  .emits(doc.render(10).getBytes("utf-8"))
                  .through(outPipe)
                  .compile
                  .drain
              } yield ExitCode.Success

            case Validate(file, wat, dev) =>
              val throwableFormat =
                if (dev)
                  ThrowableFormat(ThrowableFormat.Depth.Full, ThrowableFormat.Indent.Fixed(2))
                else
                  ThrowableFormat(ThrowableFormat.Depth.Fixed(0), ThrowableFormat.Indent.NoIndent)
              val formatter = Formatter.create(throwableFormat, true)
              val logger = consoleLogger[IO](formatter = formatter)
              for {
                engine <- Engine[IO](blocker)
                tcompiler <- Compiler[IO](blocker)
                module = if (wat) tcompiler.stream(file, false, blocker) else engine.sections(file, blocker)
                res <- engine.validate(module).attempt
                _ <- res.fold(t => logger.error("Module is invalid", t), _ => logger.info("Module is valid"))
              } yield ExitCode.Success
            case Infer(file, wat, func_name) =>
              for {
                engine <- Engine[IO](blocker)
                tcompiler <- swam.text.Compiler[IO](blocker)
                module = if (wat) tcompiler.stream(file, false, blocker) else engine.sections(file, blocker)

                compiled <- engine.compile(module)
                names <- IO(compiled.names.flatMap(_.subsections.collectFirst { case FunctionNames(n) => n }))
                exitCode <- IO(
                  names match {
                    case Some(x) => {
                      val func = x.filter { case (idx, name) => func_name == name }

                      if (func.nonEmpty) {

                        if (func.size > 1) {

                          System.err.println(s"Warning $func_name has more than one definition, taking the first one")
                        }

                        val tpeidx = func.collectFirst { case (tid, _) => tid }.get

                        // There is always one at this point
                        val tpe = compiled.functions.filter(f => f.idx == tpeidx)(0).tpe

                        val params = tpe.params.map {
                          case I32 => "Int32"
                          case I64 => "Int64"
                          case F32 => "Float32"
                          case F64 => "Float64"
                        }
                        println(params.mkString(","))
                        ExitCode.Success
                      } else {
                        System.err.println(s"Function '$func_name' does not exist. Listing available functions...")

                        names.foreach(f => f.foreach(e => println(s"\t${e._2}")))

                        ExitCode.Error
                      }
                    }
                    case None => {
                      System.err.println(s"The module does not contain a name/metadata section")
                      ExitCode.Error
                    }
                  }
                )

              } yield exitCode
            case Compile(file, out, debug) =>
              for {
                tcompiler <- Compiler[IO](blocker)
                _ <- (Stream.emits(ModuleStream.header.toArray) ++ tcompiler
                  .stream(file, debug, blocker)
                  .through(ModuleStream.encoder.encode[IO])
                  .flatMap(bv => Stream.emits(bv.toByteArray)))
                  .through(fs2.io.file.writeAll(out, blocker, outFileOptions))
                  .compile
                  .drain
              } yield ExitCode.Success

          }
        }
      }

  def prepareFunction(module: Module[IO],
                      functionName: String,
                      preopenedDirs: List[Path],
                      args: List[String],
                      useWasi: Boolean,
                      blocker: Blocker): IO[Function[IO]] = {
    val logger = consoleLogger[IO]()
    if (useWasi) {
      Wasi[IO](preopenedDirs, functionName :: args, logger, blocker).use { wasi =>
        for {
          instance <- module.importing("wasi_snapshot_preview1", wasi).instantiate
          exportedFunc <- instance.exports.function(functionName)
          memory <- instance.exports.memory("memory")
          _ <- wasi.mem.complete(memory)

        } yield exportedFunc
      }
    } else {
      for {
        instance <- module.instantiate
        exportedFunc <- instance.exports.function(functionName)
      } yield exportedFunc
    }
  }

  def executeFunction(preparedFunction: IO[Function[IO]], parameters: Vector[Value], time: Boolean): Vector[Value] = {
    val logger = consoleLogger[IO]()
    Blocker[IO]
      .use { _ =>
        for {
          f <- preparedFunction
          res <- if (time) measureTime(logger, f.invoke(parameters, None)) else f.invoke(parameters, None)
        } yield res
      }
      .unsafeRunSync()
  }

  def measureTime[T](logger: Logger[IO], io: IO[T]): IO[T] =
    for {
      start <- Clock[IO].monotonic(TimeUnit.NANOSECONDS)
      res <- io
      end <- Clock[IO].monotonic(TimeUnit.NANOSECONDS)
      _ <- logger.info(s"Execution took ${end - start}ns")
    } yield res

  // Create the required input vector for the instantiated Wasm function
  def parseWasmArgs(argsTypes: List[String], args: List[String]): Vector[Value] = {
    if (argsTypes.length != args.length)
      throw new Exception("Number of args not equal to number of arg types!")
    argsTypes.zipWithIndex.map {
      case ("Int32", index) =>
        Value.Int32(args(index).toInt)
      case ("Int64", index) =>
        Value.Int64(args(index).toLong)
      case ("Float32", index) =>
        Value.Float32(args(index).toFloat)
      case ("Float64", index) =>
        Value.Float64(args(index).toDouble)
      case (unknownType, _) =>
        throw new Exception("Type does not exist for Wasm: " + unknownType)
    }.toVector
  }
}
