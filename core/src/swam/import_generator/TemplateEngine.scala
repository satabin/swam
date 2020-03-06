package swam.import_generator

import swam.FuncType
import swam.syntax.Section.Imports

import scala.io.Source

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  */
class TemplateEngine private {

  def getFileContent(fileName: String): String = Source.fromResource(s"templates/$fileName").mkString("")

  def render(templateName: String, context: Map[String, String]): String = {

    val toReplaceRe = "\\{(\\w+)}".r

    toReplaceRe.replaceAllIn(getFileContent(templateName), (m) => {
      if (context.contains(m.group(1))) {
        context(m.group(1))
      } else
        "UNKNOWN"
    })
  }
}

object TemplateEngine {
  def apply(): TemplateEngine = new TemplateEngine()
}
