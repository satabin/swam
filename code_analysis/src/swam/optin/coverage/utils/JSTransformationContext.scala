package swam
package code_analysis
package coverage
package utils

import swam.syntax.{Import, Section}
import swam.syntax.Section.Imports

/**
  *@author Javier Cabrera-Arteaga on 2020-10-08
  */
case class JSTransformationContext(sections: Seq[(Section, Long)],
                                   types: Option[(Section.Types, Long)],
                                   imported: Option[(Section.Imports, Long)],
                                   code: Option[(Section.Code, Long)],
                                   exports: Option[(Section.Exports, Long)],
                                   names: Option[(Section.Custom, Long)],
                                   functions: Option[(Section.Functions, Long)],
                                   elements: Option[(Section.Elements, Long)])
    extends TransformationContext {

  lazy val cbFuncIndex: Int = numberOfImportedFunctions - 1

  lazy val sortedSections: Seq[Section] =
    reduceSections(sections.map(t => Option(t)).toVector,
                   Option(redefinedTypes),
                   imported,
                   elements,
                   functions,
                   names,
                   code,
                   exports).sortBy(t => t._2).map(t => t._1)

  lazy val numberOfImportedFunctions: Int = imported.get._1.imports.collect {
    case x: Import.Function => x
  }.size

  lazy val tpeIndex: Int = types match {
    case None => {
      -1
    }
    case Some(tps) =>
      val idx = tps._1.types.zipWithIndex.filter {
        case (tpe, _) => tpe.params.size == 1 && tpe.params(0) == ValType.I32 && tpe.t.isEmpty
      }
      if (idx.isEmpty) tps._1.types.size else idx(0)._2
  }

  lazy val redefinedTypes: (Section.Types, Long) =
    if (tpeIndex == types.get._1.types.size)
      (Section.Types(types.get._1.types.appended(FuncType(Vector(ValType.I32), Vector()))), types.get._2)
    else types.get
}
