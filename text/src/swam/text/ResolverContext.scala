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
package text

import unresolved._

case class Def(id: Id, pos: Int)

case class ResolverContext(name: Id = NoId,
                           types: Vector[Def] = Vector.empty,
                           funcs: Vector[Def] = Vector.empty,
                           tables: Vector[Def] = Vector.empty,
                           mems: Vector[Def] = Vector.empty,
                           globals: Vector[Def] = Vector.empty,
                           locals: Vector[Def] = Vector.empty,
                           labels: Vector[Def] = Vector.empty,
                           datas: Vector[Def] = Vector.empty,
                           elems: Vector[Def] = Vector.empty,
                           typedefs: Vector[FuncType] = Vector.empty,
                           localNames: Vector[Vector[Id]] = Vector.empty,
                           labelNames: Vector[Vector[Id]] = Vector.empty,
                           importAllowed: Boolean = true,
                           inFunction: Boolean = false) {

  def noImport: ResolverContext =
    if (importAllowed) copy(importAllowed = false) else this

  def withLocalNames(locals: Seq[Def], debug: Boolean): ResolverContext =
    if (debug)
      copy(localNames = localNames :+ locals.map(_.id).toVector)
    else
      this

  def withLabelNames(debug: Boolean): ResolverContext =
    if (debug)
      copy(labelNames = labelNames :+ Vector.empty)
    else
      this

  def isEmpty: Boolean =
    types.isEmpty && funcs.isEmpty && tables.isEmpty && mems.isEmpty && globals.isEmpty && locals.isEmpty && labels.isEmpty && typedefs.isEmpty

  def pushLabel(lbl: Id, pos: Int, debug: Boolean): ResolverContext = {
    val labelNames1 =
      if (inFunction && debug)
        labelNames.init :+ (labelNames.last :+ lbl)
      else
        labelNames
    copy(labels = Def(lbl, pos) +: labels, labelNames = labelNames1)
  }

  def popLabel: ResolverContext =
    copy(labels = labels.tail)

}
