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
import syntax._
import binary._
import imports._
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
  * It exposes all the needed interface to interact with modules.
  *
  * You typically want to reuse the same instance for all your executions
  * over the same effectful type `F`.
  */
class SwamEngine[F[_]](val conf: EngineConfiguration = defaultConfiguration) {

  private[runtime] val validator = new SpecValidator[F](conf.data.hardMax)

  private[runtime] val compiler = new Compiler[F](this)

  private[runtime] val interpreter = new Interpreter[F](this)

  private[runtime] val instantiator = new Instantiator[F](this)

  private def readPath(path: Path): BitVector =
    BitVector.fromChannel(new java.io.FileInputStream(path.toFile).getChannel)

  private def readStream(bytes: BitVector)(implicit F: Effect[F]) =
    ModuleStream.decoder
      .decode(bytes)

  /** Reads the `.wasm` file at the given path and validates it.
    *
    * If validation fails, returns an error with the validation message wrapped in it.
    */
  def validate(path: Path)(implicit F: Effect[F]): F[Unit] =
    validate(readPath(path))

  /** Reads the given binary encoded module and validates it.
    *
    * If validation fails, returns an error with the validation message wrapped in it.
    */
  def validate(bytes: BitVector)(implicit F: Effect[F]): F[Unit] =
    validate(readStream(bytes))

  /** Reads the given stream of binary module sections and validates it.
    *
    * If validation fails, returns an error with the validation message wrapped in it.
    */
  def validate(sections: Stream[F, Section])(implicit F: Effect[F]): F[Unit] =
    sections
      .through(validator.validate)
      .compile
      .drain

  /** Reads the `.wasm` file at the given path, validates, and compiles it.
    * The returned compiled [[Module]] can then be instantiated to be run.
    *
    * If validation or compilation fails, returns an error with the
    * message wrapped in it.
    */
  def compile(path: Path)(implicit F: Effect[F]): F[Module[F]] =
    compile(readPath(path))

  /** Reads the given binary encoded module, validates, and compiles it.
    * The returned compiled [[Module]] can then be instantiated to be run.
    *
    * If validation or compilation fails, returns an error with the
    * message wrapped in it.
    */
  def compile(bytes: BitVector)(implicit F: Effect[F]): F[Module[F]] =
    compile(readStream(bytes))

  /** Reads the given stream of binary module sections, validates, and compiles it.
    * The returned compiled [[Module]] can then be instantiated to be run.
    *
    * If validation or compilation fails, returns an error with the
    * message wrapped in it.
    */
  def compile(sections: Stream[F, Section])(implicit F: Effect[F]): F[Module[F]] =
    sections
      .through(validator.validate)
      .through(compiler.compile)
      .compile
      .last
      .map(_.get)

  /** Reads the `.wasm` file at the given path, validates, compiles, and instantiates it.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If validation, compilation, or instantiation fails, returns an error with the
    * message wrapped in it.
    */
  def instantiate(path: Path, imports: Imports[F])(implicit F: Effect[F]): F[Instance[F]] =
    instantiate(readPath(path), imports)

  /** Reads the given binary encoded module, validates, compiles, and instantiates it.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If validation, compilation, or instantiation fails, returns an error with the
    * message wrapped in it.
    */
  def instantiate(bytes: BitVector, imports: Imports[F])(implicit F: Effect[F]): F[Instance[F]] =
    compile(bytes).flatMap(instantiate(_, imports))

  /** Reads the given stream of binary module sections, validates, compiles, and instantiates it.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If validation, compilation, or instantiation fails, returns an error with the
    * message wrapped in it.
    */
  def instantiate(sections: Stream[F, Section], imports: Imports[F])(implicit F: Effect[F]): F[Instance[F]] =
    compile(sections).flatMap(instantiate(_, imports))

  /** Instantiates the previously validated and compiled module.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If instantiation fails, returns an error with the message wrapped in it.
    */
  def instantiate(module: Module[F], imports: Imports[F])(implicit F: Effect[F]): F[Instance[F]] =
    instantiator.instantiate(module, imports)

}
