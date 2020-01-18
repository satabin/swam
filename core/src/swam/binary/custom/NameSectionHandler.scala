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
package binary
package custom

import scodec.Codec
import scodec.codecs._

case class Names(subsections: Vector[NameSubsection])

sealed trait NameSubsection
case class ModuleName(name: String) extends NameSubsection
case class FunctionNames(names: Map[Int, String]) extends NameSubsection
case class LocalNames(names: Map[Int, Map[Int, String]]) extends NameSubsection

/** Handles the custom name section as defined in the specification.
  *  This may be used to help debug webassembly code.
  */
object NameSectionHandler extends CustomSectionHandler[Names] {

  val handleName = "name"

  private val name = variableSizeBytes(varuint32, utf8)

  private val namemap = variableSizeBytes(varuint32, vectorOfN(varuint32, varuint32 ~ name))

  private val indirectnamemap = variableSizeBytes(varuint32, vectorOfN(varuint32, varuint32 ~ namemap))

  private val subsection: Codec[NameSubsection] =
    discriminated[NameSubsection]
      .by(byte)
      .|(0) { case ModuleName(n) => n }(ModuleName(_))(name)
      .|(1) { case FunctionNames(ns) => ns.toVector }(v => FunctionNames(v.toMap))(namemap)
      .|(2) { case LocalNames(ns) => ns.view.mapValues(_.toVector).toVector }(v =>
        LocalNames(v.map { case (k, v) => (k, v.toMap) }.toMap))(indirectnamemap)

  val codec: Codec[Names] = vector(subsection).as[Names]

}
