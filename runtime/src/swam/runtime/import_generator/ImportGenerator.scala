package swam.runtime.import_generator

import swam.FuncType
import swam.import_generator.TemplateEngine
import swam.runtime.Import

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  */
/**
  * Generates the scala code with the template to use as Import in the WASM execution
  */
class ImportGenerator private {

  val renderer = TemplateEngine()

  def generateImportText(imports: Vector[Import]) = {
    renderer.render("ImportTemplate.sc", Map())
  }
}

object ImportGenerator {

  def make(): ImportGenerator =
    new ImportGenerator

}
