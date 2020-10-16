package swam
package code_analysis
package coverage
package utils

import swam.syntax.{Import, Section, i32}

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
                                      elements: Option[(Section.Elements, Long)],
                                      data: Option[(Section.Datas, Long)],
                                      globals: Option[(Section.Globals, Long)],
                                      cbFuncIndex: Int,
                                      previousIdGlobalIndex: Int,
                                      blockCount: Int,
                                      AFLCoverageSize: Int) {

  def reduceSections(prepend: Vector[Option[(Section, Long)]],
                     sections: Option[(Section, Long)]*): Seq[(Section, Long)] = {

    val v: Vector[(Section, Long)] = Vector()
    prepend.concat(sections).foldLeft(v) {
      case (cumul, o) =>
        o match {
          case Some(x) => cumul.appended(x)
          case None    => cumul
        }
    }
  }

  lazy val lastDataOffsetAndLength: (Long, Int) =
    data match {
      case Some(n) =>
        n match {
          case (d: Section.Datas, _) =>
            d.data.last.offset(0) match {
              case i32.Const(v) => (v + d.data.last.init.length, d.data.last.data)
              case _            => throw new Exception("Should data offset be a constant ?")
            }
        }
      case None => (0, 0)
    }

  lazy val blockCoverageDataOffsetAndLength: (Long, Int) =
    (lastDataOffsetAndLength._1 + AFLCoverageSize, blockCount + 1)

  lazy val sortedSections: Seq[Section] =
    reduceSections(sections.map(t => Option(t)).toVector,
                   Option(redefinedTypes),
                   imported,
                   globals,
                   data,
                   elements,
                   functions,
                   names,
                   code,
                   exports).sortBy(t => t._2).map(t => t._1)

  /*names match {
      case Some(m) =>
        (redefinedTypes +: imported.get +: globals.get +: data.get +: elements.get +: functions.get +: m +: sections :+ code.get :+ exports.get)
          .sortBy(t => t._2)
          .map(t => t._1)
      case None =>
        (redefinedTypes +: imported.get +: globals.get +: data.get +: elements.get +: functions.get +: sections :+ code.get :+ exports.get)
          .sortBy(t => t._2)
          .map(t => t._1)

    }*/

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
