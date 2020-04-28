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

/** Base class for anything that requires reading a module from stream or file.
  */
class ModuleLoader[F[_]](implicit F: Sync[F]) {

  /** Reads a binary module from the given path. */
  def sections(path: Path, blocker: Blocker, chunkSize: Int = 1024)(implicit cs: ContextShift[F]): Stream[F, Section] =
    sections(io.file.readAll(path, blocker, chunkSize = chunkSize))

  /** Reads a binary module from the given bytes. */
  def sections(bytes: Stream[F, Byte]): Stream[F, Section] = {
    def go(s: Stream[F, Byte]): Pull[F, Section, Unit] =
      ModuleStream.decoder(s.chunks.map(_.toBitVector)).flatMap {
        case Some(_) => Pull.raiseError(new BinaryException("unexpected end of input"))
        case None    => Pull.done
      }

    bytes.pull
      .unconsN(8, allowFewer = false)
      .flatMap {
        case Some((headerBytes, tl)) =>
          val bv = ByteVector(headerBytes.toArray)
          if (bv == ModuleStream.header)
            Pull.pure(tl)
          else
            Pull.raiseError(new BinaryException(s"unexpected header ${bv.toHex(Bases.Alphabets.HexUppercase)}"))
        case None => Pull.raiseError(new BinaryException("unexpected end of input"))
      }
      .flatMap(go(_))
      .stream
  }

}
