package swam
package code_analysis
package coverage
package utils

import swam.syntax.Section

/**
  * @author Javier Cabrera-Arteaga on 2020-10-18
  */
abstract class TransformationContext {

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
}
