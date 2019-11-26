/*
 * Copyright 2019 Lucas Satabin
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
package traversal

import syntax._

import cats._

class SectionTraverser[F[_], Res](implicit F: Applicative[F]) {

  protected[this] val fst = (outer: Res, i: Section) => F.pure(outer)

  val typesTraverse: (Res, Section.Types) => F[Res] = fst
  val importsTraverse: (Res, Section.Imports) => F[Res] = fst
  val functionsTraverse: (Res, Section.Functions) => F[Res] = fst
  val tablesTraverse: (Res, Section.Tables) => F[Res] = fst
  val memoriesTraverse: (Res, Section.Memories) => F[Res] = fst
  val globalsTraverse: (Res, Section.Globals) => F[Res] = fst
  val exportsTraverse: (Res, Section.Exports) => F[Res] = fst
  val startTraverse: (Res, Section.Start) => F[Res] = fst
  val elementsTraverse: (Res, Section.Elements) => F[Res] = fst
  val codeTraverse: (Res, Section.Code) => F[Res] = fst
  val datasTraverse: (Res, Section.Datas) => F[Res] = fst
  val customTraverse: (Res, Section.Custom) => F[Res] = fst

  final def traverse(outer: Res, sec: Section): F[Res] = sec match {
    case sec @ Section.Types(_)     => typesTraverse(outer, sec)
    case sec @ Section.Imports(_)   => importsTraverse(outer, sec)
    case sec @ Section.Functions(_) => functionsTraverse(outer, sec)
    case sec @ Section.Tables(_)    => tablesTraverse(outer, sec)
    case sec @ Section.Memories(_)  => memoriesTraverse(outer, sec)
    case sec @ Section.Globals(_)   => globalsTraverse(outer, sec)
    case sec @ Section.Exports(_)   => exportsTraverse(outer, sec)
    case sec @ Section.Start(_)     => startTraverse(outer, sec)
    case sec @ Section.Elements(_)  => elementsTraverse(outer, sec)
    case sec @ Section.Code(_)      => codeTraverse(outer, sec)
    case sec @ Section.Datas(_)     => datasTraverse(outer, sec)
    case sec @ Section.Custom(_, _) => customTraverse(outer, sec)
  }

}
