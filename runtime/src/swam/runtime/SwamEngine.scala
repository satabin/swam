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
package runtime

import config._
import binary._
import validation._
import internals.compiler._
import internals.instance._
import internals.interpreter._

import cats._
import cats.implicits._
import cats.effect._

import fs2._

import scodec.bits._

import scala.language.higherKinds

import java.nio.file.Path

/** This is the engine used to compile, instantiate and run modules.
  *  It exposes all the needed interface to interact with modules.
  */
class SwamEngine[F[_]](val conf: EngineConfiguration = defaultConfiguration)(implicit F: Effect[F]) {

  private[runtime] val validator = new SpecValidator[F]

  private[runtime] val compiler = new Compiler[F](this)

  private[runtime] val interpreter = new Interpreter[F](this)

  private[runtime] val instantiator = new Instantiator[F](this)

  private def readPath(path: Path): BitVector =
    BitVector.fromChannel(new java.io.FileInputStream(path.toFile).getChannel)

  private def readStream(bytes: BitVector) =
    ModuleStream.decoder
      .decode(bytes)

  def validate(path: Path): F[Unit] =
    validate(readPath(path))

  def validate(bytes: BitVector): F[Unit] =
    readStream(bytes)
      .through(validator.validate)
      .compile
      .drain

  def compile(path: Path): F[Module[F]] =
    compile(readPath(path))

  def compile(bytes: BitVector): F[Module[F]] =
    readStream(bytes)
      .through(validator.validate)
      .through(compiler.compile)
      .compile
      .last
      .map(_.get)

  def instantiate(path: Path, imports: Imports[F]): F[Instance[F]] =
    instantiate(readPath(path), imports)

  def instantiate(bytes: BitVector, imports: Imports[F]): F[Instance[F]] =
    compile(bytes).flatMap(instantiate(_, imports))

  def instantiate(module: Module[F], imports: Imports[F]): F[Instance[F]] =
    instantiator.instantiate(module, imports)

}
