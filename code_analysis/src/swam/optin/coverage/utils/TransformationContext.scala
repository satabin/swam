package swam
package code_analysis
package coverage
package utils

import swam.syntax.{Import, Section}
import swam.syntax.Section.Imports

/**
  *@author Javier Cabrera-Arteaga on 2020-10-08
  */
case class TransformationContext(sections: Seq[Section],
                                 types: Option[Section.Types],
                                 imported: Option[Section.Imports],
                                 code: Option[Section.Code],
                                 exports: Option[Section.Exports],
                                 names: Option[Section.Custom]) {

  lazy val cbFuncIndex: Int = numberOfImportedFunctions - 1

  lazy val sortedSections: Seq[Section] =
    names match {
      case Some(m) => (redefinedTypes +: sections :+ code.get :+ imported.get :+ exports.get :+ m).sortBy(t => t.id)
      case None    => (redefinedTypes +: sections :+ code.get :+ imported.get :+ exports.get).sortBy(t => t.id)

    }

  lazy val numberOfImportedFunctions: Int = imported.get.imports.collect {
    case x: Import.Function => x
  }.size

  lazy val tpeIndex: Int = types match {
    case None => {
      -1
    }
    case Some(tps) =>
      val idx = tps.types.zipWithIndex.filter {
        case (tpe, _) => tpe.params.size == 1 && tpe.params(0) == ValType.I32 && tpe.t.isEmpty
      }
      if (idx.isEmpty) tps.types.size else idx(0)._2
  }

  lazy val redefinedTypes: Section.Types =
    if (tpeIndex == types.get.types.size)
      Section.Types(types.get.types.appended(FuncType(Vector(ValType.I32), Vector())))
    else types.get
}
