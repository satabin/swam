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
package validation

import syntax._
import config._

import cats._
import cats.effect._
import cats.implicits._

import pureconfig._
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

import fs2._

/** A validator makes it possible to validate sections in a stream. */
abstract class Validator[F[_]] {

  /** Performs validation of the section stream on the fly.
    *  The sections are returned unchanged if validation succeeds, otherwise
    *  the stream fails.
    */
  def validate(stream: Stream[F, Section]): Stream[F, Section]

}

object Validator {
  def apply[F[_]: MonadError[?[_], Throwable]](conf: ValidationConfiguration): Validator[F] =
    if (conf.validate)
      new SpecValidator[F](conf.hardMax.bytes.toInt)
    else
      new NoopValidator[F]

  def apply[F[_]: Sync]: F[Validator[F]] =
    for {
      conf <- ConfigSource.default.at("swam.validation").loadF[F, ValidationConfiguration]
    } yield apply[F](conf)
}
