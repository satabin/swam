/*
 * Copyright 2018 Lucas Satabin
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

import util._
import text._
import text.parser._
import binary._
import validation._

import cats.effect._
import cats.implicits._

import com.monovore.decline._

import java.nio.file.Paths

import fastparse.core._

object TextMain
    extends CommandApp(
      name = "swamt",
      header = ".wast file manipulation",
      main = {

        val verbose = Opts.flag("verbose", help = "Print extra metadata to the console.").orFalse

        val files = Opts.arguments[String]("file")

        val checkCommand =
          Opts.subcommand("check", help = "Only check the .wast files, without generating binary format.") {

            val parse = Opts
              .flag("parse", help = "Only parse the .wast files, reporting syntax errors")
              .map(_ => CheckStage.Parse)
            val resolve = Opts
              .flag("resolve",
                    help = "Only parse and resolve names in the .wast files, reporting syntax and resolution errors")
              .map(_ => CheckStage.Resolve)

            val stage = parse.orElse(resolve).withDefault(CheckStage.All)

            (files, stage, verbose).mapN { (files, stage, v) =>
              files.foldLeft(()) { (_, file) =>
                Checker.check(file, stage, v)
              }
            }
          }

        val compileCommand = Opts.subcommand("compile", help = "Generate binary representation of the .wast files.") {
          verbose.map { v =>
            }
        }

        checkCommand.orElse(compileCommand)
      }
    )

object Checker {

  private val resolver = new Resolver[IO]

  private val validator = new SpecValidator[IO]

  private val binaryParser = new SwamParser[IO]

  def check(file: String, stage: CheckStage, verbose: Boolean) = {
    if (verbose)
      println(s"Checking file $file")
    val io =
      stage match {
        case CheckStage.Parse =>
          for {
            input <- readFile(Paths.get(file))
            _ <- parse(input)
          } yield ()
        case CheckStage.Resolve =>
          for {
            input <- readFile(Paths.get(file))
            unresolved <- parse(input)
            _ <- resolver.resolve(unresolved)
          } yield ()
        case CheckStage.All =>
          for {
            input <- readFile(Paths.get(file))
            unresolved <- parse(input)
            resolved <- resolver.resolve(unresolved)
            _ <- binaryParser.parse(resolved, validator)
          } yield ()
      }

    try {
      io.unsafeRunSync()
    } catch {
      case f @ ParseError(e) =>
        val positioner = new WastPositioner(Paths.get(file))
        val TextFilePosition(path, line, column, _) = positioner.render(e.index)
        println(s"in file $path:\n[$line.$column]: ${f.getMessage}")
      case e @ TextCompilerException(msg, positions) =>
        val positioner = new WastPositioner(Paths.get(file))
        println(s"in file ${Paths.get(file)}:\n${e.getMessage}")
        for(pos <- positions) {
          val TextFilePosition(path, line, column, content) = positioner.render(pos)
          val prefix = s"[$line.$column]: "
          print(prefix)
          println(content)
          print(" " * (column - 1 + prefix.size))
          println("^")
        }
    }
  }

  private def parse(input: String): IO[unresolved.Module] =
    IO(ModuleParsers.file.parse(input)).flatMap {
      case Parsed.Success(m, _)        => IO.pure(m)
      case f @ Parsed.Failure(_, _, _) => IO.raiseError(ParseError(f))
    }

}

sealed trait CheckStage
object CheckStage {
  case object Parse extends CheckStage
  case object Resolve extends CheckStage
  case object All extends CheckStage
}
