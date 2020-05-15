/*
 * Copyright 2020 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam
package cli

import decompilation._
import runtime._
import binary.ModuleStream
import runtime.imports._
import runtime.trace._
import runtime.wasi.Wasi

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

import io.odin._

import fs2._

import java.util.logging.{Formatter, LogRecord}
import java.nio.file._

private object NoTimestampFormatter extends Formatter {
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

  val decompileOpts: Opts[Options] = Opts.subcommand("decompile", "Decompile a wasm file") {
    (textual, wasmFile, out.orNone).mapN { (textual, wasm, out) => Decompile(wasm, textual, out) }
  }

  val validateOpts: Opts[Options] = Opts.subcommand("validate", "Validate a wasm file") {
    wasmFile.map(Validate(_))
  }

  val compileOpts: Opts[Options] = Opts.subcommand("compile", "Compile a wat file to wasm") {
    (wasmFile, out, debug).mapN(Compile(_, _, _))
  }

  val logger = consoleLogger[IO]()

  def measureTime[T](io: IO[T]): IO[T] =
    for {
      start <- Clock[IO].monotonic(TimeUnit.NANOSECONDS)
      res <- io
      end <- Clock[IO]monotonic(TimeUnit.NANOSECONDS)
      _ <- logger.info(s"Execution took ${end - start}ns")
    } yield res

  def doRun(module: Module[IO],
            main: String,
            preopenedDirs: List[Path],
            args: List[String],
            wasi: Boolean,
            time: Boolean,
            blocker: Blocker): IO[Unit] =
    if (wasi)
      Wasi[IO](preopenedDirs, main :: args, logger, blocker).use { wasi =>
        for {
          instance <- module.importing("wasi_snapshot_preview1", wasi).instantiate
          main <- instance.exports.typed.procedure0(main)
          memory <- instance.exports.memory("memory")
          _ <- wasi.mem.complete(memory)
          _ <- if (time) measureTime(main()) else main()
        } yield ()
      }
    else
      for {
        instance <- module.instantiate
        main <- instance.exports.typed.procedure0(main)
        _ <- if (time) measureTime(main()) else main()
      } yield ()

  val outFileOptions = List(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

  def main: Opts[IO[ExitCode]] =
    runOpts.orElse(decompileOpts).orElse(validateOpts).orElse(compileOpts).map { opts =>
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
              _ <- doRun(compiled, main, dirs, args, wasi, time, blocker)
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
          case Validate(file) =>
            for {
              engine <- Engine[IO](blocker)
              res <- engine.validate(file, blocker).attempt
              msg = res.fold(t => s"Module is invalid:\n${t.getMessage()}", _ => "Module is valid")
              _ <- Stream
                .emits(msg.getBytes("utf-8"))
                .through(fs2.io.stdout[IO](blocker))
                .compile
                .drain
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
