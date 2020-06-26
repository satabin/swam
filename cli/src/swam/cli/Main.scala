package swam
package cli

import java.nio.file._
import java.util.concurrent.TimeUnit
import java.util.logging.{LogRecord, Formatter => JFormatter}

import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import io.odin._
import io.odin.formatter.Formatter
import io.odin.formatter.options.ThrowableFormat
import fs2._
import swam.binary.ModuleStream
import swam.decompilation._
import swam.runtime.imports._
import swam.runtime.trace._
import swam.runtime.wasi.Wasi
import swam.runtime.{Engine, Module}
import swam.optin.coverage.{CoverageListener, CoverageReporter}

private object NoTimestampFormatter extends JFormatter {
  override def format(x: LogRecord): String =
    x.getMessage
}

object Main extends CommandIOApp(name = "swam-cli", header = "Swam from the command line") {

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  val wasmFile =
    Opts.argument[Path](metavar = "wasm")

  val restArguments =
    Opts.arguments[String](metavar = "args").orEmpty

  val dirs = Opts
    .options[Path]("dir", "Preopen directory", short = "D", metavar = "dir")
    .orEmpty

  val mainFun =
    Opts
      .option[String]("main", "Execute provided function name as main (default _start)", short = "m")
      .withDefault("_start")

  val wat =
    Opts.flag("wat", "Input file is in WAT format, and needs to be parsed as such (default false)", short = "w").orFalse

  val wasi =
    Opts.flag("wasi", "Program is using wasi (default false)", short = "W").orFalse

  val time =
    Opts.flag("time", "Measure execution time (default false)", short = "C").orFalse

  val trace =
    Opts.flag("trace", "Trace WASM execution channels (default false)", short = "t").orFalse

  val cov =
    Opts.flag("instcov", "Run the WebAssembly module and gets coverage.", short = "v").orFalse

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

  val runOpts: Opts[Options] = Opts.subcommand("run", "Run a WebAssembly file") {
    (mainFun, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasmFile, restArguments).mapN {
      (main, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasm, args) =>
        Run(wasm, args, main, wat, wasi, time, trace, filter, traceFile, dirs, debug)
    }
  }

  val covOpts: Opts[Options] = Opts.subcommand("coverage", "Run a WebAssembly file and generate coverage report") {
    (mainFun, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasmFile, restArguments, cov, out).mapN {
      (main, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasm, args, cov, out) =>
        RunWithCov(wasm, args, main, wat, wasi, time, trace, filter, traceFile, dirs, debug, cov, out)
    }
  }

  val decompileOpts: Opts[Options] = Opts.subcommand("decompile", "Decompile a wasm file") {
    (textual, wasmFile, out.orNone).mapN { (textual, wasm, out) => Decompile(wasm, textual, out) }
  }

  val validateOpts: Opts[Options] = Opts.subcommand("validate", "Validate a wasm file") {
    (wasmFile, wat, dev).mapN(Validate(_, _, _))
  }

  val compileOpts: Opts[Options] = Opts.subcommand("compile", "Compile a wat file to wasm") {
    (wasmFile, out, debug).mapN(Compile(_, _, _))
  }

  val serverOpts: Opts[Options] = Opts.subcommand("run_server", "Run a TCP server for a given WASM that listens to inputs") {
    // TODO: Check which ones of these are really necessary
    (mainFun, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasmFile, restArguments, cov, out).mapN {
      (main, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasm, args, cov, out) =>
        RunServer(wasm, args, main, wat, wasi, time, trace, filter, traceFile, dirs, debug, cov, out)
    }
  }

  def measureTime[T](logger: Logger[IO], io: IO[T]): IO[T] =
    for {
      start <- Clock[IO].monotonic(TimeUnit.NANOSECONDS)
      res <- io
      end <- Clock[IO].monotonic(TimeUnit.NANOSECONDS)
      _ <- logger.info(s"Execution took ${end - start}ns")
    } yield res

    def prepareFunction(module: Module[IO],
            function_name: String,
            preopenedDirs: List[Path],
            args: List[String],
            useWasi: Boolean,
            blocker: Blocker): IO[() => IO[Unit]] = {
        val logger = consoleLogger[IO]()
        if (useWasi) {
            Wasi[IO](preopenedDirs, function_name :: args, logger, blocker).use { wasi =>
                for {
                    instance <- module.importing("wasi_snapshot_preview1", wasi).instantiate
                    exportedFunc <- instance.exports.typed.function[Unit, Unit](function_name)
                    memory <- instance.exports.memory("memory")
                    _ <- wasi.mem.complete(memory)
                } yield exportedFunc
            }
        } else {
            for {
                instance <- module.instantiate
                exportedFunc <- instance.exports.typed.function[Unit, Unit](function_name)
            } yield exportedFunc
        }
    }

  def executeFunction(preparedFunction: () => IO[Unit], time: Boolean): IO[Unit] = {
    val logger = consoleLogger[IO]()
    if (time)
      measureTime(logger, preparedFunction())
    else
      preparedFunction()
  }

  val outFileOptions = List(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

  def main: Opts[IO[ExitCode]] =
    runOpts.orElse(serverOpts).orElse(covOpts).orElse(decompileOpts).orElse(validateOpts).orElse(compileOpts).map { opts =>
      Blocker[IO].use { blocker =>
        opts match {
          case Run(file, args, main, wat, wasi, time, trace, filter, tracef, dirs, debug) =>
            for {
              tracer <- if (trace)
                JULTracer[IO](blocker,
                              traceFolder = ".",
                              traceNamePattern = tracef.toAbsolutePath().toString(),
                              filter = filter,
                              formatter = NoTimestampFormatter).map(Some(_))
              else
                IO(None)
              engine <- Engine[IO](blocker, tracer)
              tcompiler <- swam.text.Compiler[IO](blocker)
              module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
              compiled <- engine.compile(module)

              preparedFunction <- prepareFunction(compiled, main, dirs, args, wasi, blocker)
              _ <- executeFunction(preparedFunction, time)
            } yield ExitCode.Success

            // TODO: Delete this and make Option(coverageListener) in .instCoverage()
          case RunWithCov(file, args, main, wat, wasi, time, trace, filter, tracef, dirs, debug, coverage, out) =>
            for {
              tracer <- if (trace)
                JULTracer[IO](blocker,
                  traceFolder = ".",
                  traceNamePattern = tracef.toAbsolutePath().toString(),
                  filter = filter,
                  formatter = NoTimestampFormatter).map(Some(_))
              else
                IO(None)
              coverageListener = CoverageListener[IO]()
              engine <- Engine[IO](blocker, tracer, listener = Option(coverageListener))
              tcompiler <- swam.text.Compiler[IO](blocker)
              module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
              compiled <- engine.compile(module)
              preparedFunction <- prepareFunction(compiled, main, dirs, args, wasi, blocker)
              _ <- executeFunction(preparedFunction, time)
              _ <- if (coverage) IO(CoverageReporter.instCoverage(Option(out), file, coverageListener, coverage)) else IO(None)
            } yield ExitCode.Success

          case RunServer(file, args, main, wat, wasi, time, trace, filter, tracef, dirs, debug, coverage, out) =>
            for {
              tracer <- if (trace)
                JULTracer[IO](blocker,
                  traceFolder = ".",
                  traceNamePattern = tracef.toAbsolutePath().toString(),
                  filter = filter,
                  formatter = NoTimestampFormatter).map(Some(_))
              else
                IO(None)
              coverageListener = CoverageListener[IO]()
              engine <- Engine[IO](blocker, tracer, listener = Option(coverageListener))
              tcompiler <- swam.text.Compiler[IO](blocker)
              module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
              compiled <- engine.compile(module)

              preparedFunction <- prepareFunction(compiled, main, dirs, args, wasi, blocker)
              _ <- IO(Server.listen(preparedFunction, time, Option(out), file, coverageListener, coverage))

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
              tcompiler <- swam.text.Compiler[IO](blocker)
              module = if (wat) tcompiler.stream(file, false, blocker) else engine.sections(file, blocker)
              res <- engine.validate(module).attempt
              _ <- res.fold(t => logger.error("Module is invalid", t), _ => logger.info("Module is valid"))
            } yield ExitCode.Success

          case Compile(file, out, debug) =>
            for {
              tcompiler <- swam.text.Compiler[IO](blocker)
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
}
