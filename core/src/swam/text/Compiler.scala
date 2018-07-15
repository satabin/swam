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
package text

import util._
import parser._
import syntax._
import binary._
import validation._

import cats.effect._
import cats.implicits._

import scala.language.higherKinds

import java.nio.file.Path

import fastparse.core._

class Compiler[F[_]](implicit val F: Effect[F]) {

  private val resolver = new Resolver[F]

  private implicit val validator = new SpecValidator[F]

  private val binaryParser = new SwamParser[F]

  def compile(file: Path): F[Module] =
    for {
      input <- F.liftIO(readFile(file))
      unresolved <- parse(input)
      mod <- compile(unresolved)
    } yield mod

  def compile(module: unresolved.Module): F[Module] =
    for {
      resolved <- resolver.resolve(module)
      mod <- binaryParser.parse(resolved, validator)
    } yield mod

  private def parse(input: String): F[unresolved.Module] =
    F.liftIO {
      IO(ModuleParsers.file.parse(input)).flatMap {
        case Parsed.Success(m, _)        => IO.pure(m)
        case f @ Parsed.Failure(_, _, _) => IO.raiseError(ParseError(f))
      }
    }

}
