package swam
package code_analysis
package coverage
package utils

import swam.syntax.{Import, Section}

/**
  *@author Javier Cabrera-Arteaga on 2020-10-08
  */
case class InnerTransformationContext(sections: Seq[(Section, Long)],
                                      types: Option[(Section.Types, Long)],
                                      imported: Option[(Section.Imports, Long)],
                                      code: Option[(Section.Code, Long)],
                                      exports: Option[(Section.Exports, Long)],
                                      names: Option[(Section.Custom, Long)],
                                      functions: Option[(Section.Functions, Long)],
                                      elements: Option[(Section.Elements, Long)]) {

  lazy val cbFuncIndex: Int = functions.get._1.functions.length

  lazy val sortedSections: Seq[Section] =
    names match {
      case Some(m) =>
        (redefinedTypes +: imported.get +: elements.get +: functions.get +: m +: sections :+ code.get :+ exports.get)
          .sortBy(t => t._2)
          .map(t => t._1)
      case None =>
        (redefinedTypes +: imported.get +: elements.get +: functions.get +: sections :+ code.get :+ exports.get)
          .sortBy(t => t._2)
          .map(t => t._1)

    }

  lazy val tpeIndex: Int = types match {
    case None => {
      0
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
