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
import util.pretty.Doc

import cats.effect._

import fs2._

import java.nio.file.Path

/** Generic class to implement a decompiler. It takes a module in binary format
  * and outputs a formatted [[swam.util.pretty.Doc Doc]] out of it.
  */
abstract class Decompiler[F[_]](implicit F: Effect[F]) extends ModuleLoader[F] {

  /** Returns a pretty-printed [[swam.util.pretty.Doc Doc]] resulting from decompiling
    * the module at the given path.
    *
    * The module is not validated so invalid modules can also be decompiled.
    */
  def decompilePath(path: Path, blocker: Blocker, chunkSize: Int = 1024)(implicit cs: ContextShift[F]): F[Doc] =
    decompile(readPath(path, blocker, chunkSize))

  /** Returns a pretty-printed [[swam.util.pretty.Doc Doc]] resulting from decompiling
    * the module at the given bytes.
    *
    * The module is not validated so invalid modules can also be decompiled.
    */
  def decompileBytes(bytes: Stream[F, Byte]): F[Doc] =
    decompile(readBytes(bytes))

  /** Returns a pretty-printed [[swam.util.pretty.Doc Doc]] resulting from decompiling
    * the module at the given section stream.
    *
    * The module is not validated so invalid modules can also be decompiled.
    */
  def decompile(sections: Stream[F, Section]): F[Doc]

}
