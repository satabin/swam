package swam
package text

import unresolved.Id

case class Def(id: Id, pos: Int)

case class ResolverContext(types: Vector[Def] = Vector.empty,
                           funcs: Vector[Def] = Vector.empty,
                           tables: Vector[Def] = Vector.empty,
                           mems: Vector[Def] = Vector.empty,
                           globals: Vector[Def] = Vector.empty,
                           locals: Vector[Def] = Vector.empty,
                           labels: Vector[Def] = Vector.empty,
                           typedefs: Vector[FuncType] = Vector.empty,
                           importAllowed: Boolean = true) {

  def noImport: ResolverContext =
    if (importAllowed) copy(importAllowed = false) else this

  def isEmpty: Boolean =
    types.isEmpty && funcs.isEmpty && tables.isEmpty && mems.isEmpty && globals.isEmpty && locals.isEmpty && labels.isEmpty && typedefs.isEmpty

  def pushLabel(lbl: Id, pos: Int): ResolverContext =
    copy(labels = Def(lbl, pos) +: labels)

  def popLabel: ResolverContext =
    copy(labels = labels.tail)

}
