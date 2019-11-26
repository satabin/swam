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
import cats.implicits._

class SectionTransformer[F[_], Ctx](implicit F: Applicative[F]) {

  protected[this] val id: (Ctx, Section) => F[(Ctx, Option[Section])] = (ctx: Ctx, s: Section) => F.pure(ctx -> s.some)

  val typesTransform: (Ctx, Section.Types) => F[(Ctx, Option[Section])] = id
  val importsTransform: (Ctx, Section.Imports) => F[(Ctx, Option[Section])] = id
  val functionsTransform: (Ctx, Section.Functions) => F[(Ctx, Option[Section])] = id
  val tablesTransform: (Ctx, Section.Tables) => F[(Ctx, Option[Section])] = id
  val memoriesTransform: (Ctx, Section.Memories) => F[(Ctx, Option[Section])] = id
  val globalsTransform: (Ctx, Section.Globals) => F[(Ctx, Option[Section])] = id
  val exportsTransform: (Ctx, Section.Exports) => F[(Ctx, Option[Section])] = id
  val startTransform: (Ctx, Section.Start) => F[(Ctx, Option[Section])] = id
  val elementsTransform: (Ctx, Section.Elements) => F[(Ctx, Option[Section])] = id
  val codeTransform: (Ctx, Section.Code) => F[(Ctx, Option[Section])] = id
  val datasTransform: (Ctx, Section.Datas) => F[(Ctx, Option[Section])] = id
  val customTransform: (Ctx, Section.Custom) => F[(Ctx, Option[Section])] = id

  final def transform(ctx: Ctx, sec: Section): F[(Ctx, Option[Section])] = sec match {
    case sec @ Section.Types(_)     => typesTransform(ctx, sec)
    case sec @ Section.Imports(_)   => importsTransform(ctx, sec)
    case sec @ Section.Functions(_) => functionsTransform(ctx, sec)
    case sec @ Section.Tables(_)    => tablesTransform(ctx, sec)
    case sec @ Section.Memories(_)  => memoriesTransform(ctx, sec)
    case sec @ Section.Globals(_)   => globalsTransform(ctx, sec)
    case sec @ Section.Exports(_)   => exportsTransform(ctx, sec)
    case sec @ Section.Start(_)     => startTransform(ctx, sec)
    case sec @ Section.Elements(_)  => elementsTransform(ctx, sec)
    case sec @ Section.Code(_)      => codeTransform(ctx, sec)
    case sec @ Section.Datas(_)     => datasTransform(ctx, sec)
    case sec @ Section.Custom(_, _) => customTransform(ctx, sec)
  }

}
