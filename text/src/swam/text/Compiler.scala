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

import parser._
import syntax._
import binary._
import validation._

import cats.effect._
import cats.implicits._

import java.nio.file.Path

import fs2._

import fastparse._

class Compiler[F[_]] private (validator: Validator[F])(implicit val F: Sync[F]) {

  private val resolver = new Resolver[F]

  private val binaryParser = new ModuleParser[F](validator)

  def compile(file: Path, blocker: Blocker, chunkSize: Int = 1024)(implicit cs: ContextShift[F]): F[Module] =
    for {
      input <- readFile(file, blocker, chunkSize)
      unresolved <- parse(input)
      mod <- compile(unresolved)
    } yield mod

  def compile(module: unresolved.Module): F[Module] =
    for {
      resolved <- resolver.resolve(module)
      mod <- binaryParser.parse(resolved)
    } yield mod

  def stream(module: unresolved.Module, debug: Boolean): Stream[F, Section] =
    Stream.force(resolver.resolve(module, debug))

  def stream(file: Path, debug: Boolean, blocker: Blocker, chunkSize: Int = 1024)(
      implicit cs: ContextShift[F]): Stream[F, Section] =
    Stream.force(for {
      input <- readFile(file, blocker, chunkSize)
      unresolved <- parse(input)
    } yield stream(unresolved, debug))

  private[swam] def parse(input: String): F[unresolved.Module] =
    F.delay(fastparse.parse(input, ModuleParsers.file(_))).flatMap {
      case Parsed.Success(m, _)          => F.pure(m)
      case f @ Parsed.Failure(_, idx, _) => F.raiseError(new ParserException(f.msg, idx))
    }

}

object Compiler {
  def apply[F[_]: Sync: ContextShift](blocker: Blocker): F[Compiler[F]] =
    for {
      validator <- Validator[F](blocker)
    } yield Compiler[F](validator)

  def apply[F[_]](validator: Validator[F])(implicit F: Sync[F]): Compiler[F] =
    new Compiler[F](validator)
}
