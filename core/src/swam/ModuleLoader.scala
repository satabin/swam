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

import binary._
import syntax._

import cats.effect._

import scodec.bits._

import fs2._

import java.nio.file.Path

import scala.language.higherKinds

/** Base class for anything that requires reading a module from stream or file.
  */
class ModuleLoader[F[_]] {

  def readPath(path: Path)(implicit F: Effect[F]): Stream[F, Section] =
    readBytes(BitVector.fromChannel(new java.io.FileInputStream(path.toFile).getChannel))

  /** Reads a binary module from the given bytes. */
  def readBytes(bytes: BitVector)(implicit F: Effect[F]): Stream[F, Section] =
    ModuleStream.decoder
      .decode(bytes)

}
