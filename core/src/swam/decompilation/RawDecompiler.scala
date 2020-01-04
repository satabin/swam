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
package decompilation

import syntax.Section
import syntax.pretty._
import util.pretty._

import cats.implicits._
import cats.effect._

import fs2._

/** A decompiler from binary format, that formats sections as they come.
  * This is a raw decompiler, not performing any validation, nor transforming
  * the binary format.
  */
class RawDecompiler[F[_]] private (implicit F: Effect[F]) extends Decompiler[F] {

  def decompile(sections: Stream[F, Section]): F[Doc] =
    sections.map(_.pretty).compile.toList.map(seq(newline, _))

}

object RawDecompiler {
  def apply[F[_]](implicit F: Effect[F]): F[RawDecompiler[F]] =
    F.pure(new RawDecompiler[F])
}
