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
import syntax.Section
import imports._
import validation._
import trace._
import internals.compiler._
import internals.instance._
import internals.interpreter._

import cats.implicits._
import cats.effect._

import fs2._

import pureconfig._
import pureconfig.generic.auto._
import pureconfig.module.catseffect.syntax._
import pureconfig.module.enumeratum._

import java.nio.file.Path

/** This is the engine used to compile, instantiate and run modules.
  * It exposes all the needed interface to interact with modules.
  *
  * You typically want to reuse the same instance for all your executions
  * over the same effectful type `F`.
  */
class Engine[F[_]: Async] private (val conf: EngineConfiguration,
                                   private[runtime] val validator: Validator[F],
                                   private[runtime] val tracer: Option[Tracer])
    extends ModuleLoader[F] {

  private[runtime] val asm =
    new Asm[F]

  private[runtime] val compiler =
    new Compiler[F](this, asm)

  private[runtime] val interpreter =
    new Interpreter[F](this)

  private[runtime] val instantiator =
    new Instantiator[F](this)

  /** Reads the `.wasm` file at the given path and validates it.
    *
    * If validation fails, returns an error with the validation message wrapped in it.
    */
  def validate(path: Path, blocker: Blocker, chunkSize: Int = 1024)(implicit cs: ContextShift[F]): F[Unit] =
    validate(sections(path, blocker, chunkSize))

  /** Reads the given binary encoded module and validates it.
    *
    * If validation fails, returns an error with the validation message wrapped in it.
    */
  def validateBytes(bytes: Stream[F, Byte]): F[Unit] =
    validate(sections(bytes))

  /** Reads the given stream of binary module sections and validates it.
    *
    * If validation fails, returns an error with the validation message wrapped in it.
    */
  def validate(sections: Stream[F, Section]): F[Unit] =
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
  def compile(path: Path, blocker: Blocker, chunkSize: Int = 1024)(implicit cs: ContextShift[F]): F[Module[F]] =
    compile(sections(path, blocker, chunkSize))

  /** Reads the given binary encoded module, validates, and compiles it.
    * The returned compiled [[Module]] can then be instantiated to be run.
    *
    * If validation or compilation fails, returns an error with the
    * message wrapped in it.
    */
  def compileBytes(bytes: Stream[F, Byte]): F[Module[F]] =
    compile(sections(bytes))

  /** Reads the given stream of binary module sections, validates, and compiles it.
    * The returned compiled [[Module]] can then be instantiated to be run.
    *
    * If validation or compilation fails, returns an error with the
    * message wrapped in it.
    */
  def compile(sections: Stream[F, Section]): F[Module[F]] =
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
  def instantiate(path: Path, imports: Imports[F], blocker: Blocker, chunkSize: Int = 1024)(
      implicit cs: ContextShift[F]): F[Instance[F]] =
    instantiate(sections(path, blocker, chunkSize), imports)

  /** Reads the given binary encoded module, validates, compiles, and instantiates it.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If validation, compilation, or instantiation fails, returns an error with the
    * message wrapped in it.
    */
  def instantiateBytes(bytes: Stream[F, Byte], imports: Imports[F]): F[Instance[F]] =
    compileBytes(bytes).flatMap(instantiate(_, imports))

  /** Reads the given stream of binary module sections, validates, compiles, and instantiates it.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If validation, compilation, or instantiation fails, returns an error with the
    * message wrapped in it.
    */
  def instantiate(sections: Stream[F, Section], imports: Imports[F]): F[Instance[F]] =
    compile(sections).flatMap(instantiate(_, imports))

  /** Instantiates the previously validated and compiled module.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If instantiation fails, returns an error with the message wrapped in it.
    */
  def instantiate(module: Module[F], imports: Imports[F]): F[Instance[F]] =
    instantiator.instantiate(module, imports)

}

object Engine {

  def apply[F[_]: Async: ContextShift](blocker: Blocker, tracer: Option[Tracer] = None): F[Engine[F]] =
    for {
      validator <- Validator[F](blocker)
      conf <- ConfigSource.default.at("swam.runtime").loadF[F, EngineConfiguration](blocker)
    } yield new Engine[F](conf, validator, tracer)

  def apply[F[_]: Async](conf: EngineConfiguration, validator: Validator[F], tracer: Option[Tracer]): Engine[F] =
    new Engine[F](conf, validator, tracer)

}
