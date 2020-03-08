package swam
package generator

import java.io.{File, PrintWriter}
import java.nio.file.Paths

import swam.{FuncType, ValType}
import swam.ValType.{F32, F64, I32, I64}
import swam.runtime.Import

import scala.reflect.runtime.universe._
import org.scalafmt.interfaces.Scalafmt

import sys.process._
import org.fusesource.scalate.{RenderContext, TemplateEngine}
import java.io.File

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  * Generates the scala code with the template to use as Import in the WASM execution
  */
class ImportGenerator {

  val f = "IO"
  val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
  val config = Paths.get(".scalafmt.conf")

  private var templateFile = getClass.getClassLoader.getResource("import_template.mustache").getFile
  var registeredFunctions: Set[String] = Set[String]()

  /**
    * Map WASM to scala primitive types
    * @param v
    * @return
    */
  private def getScalaType(v: ValType): String = v match {
    case I32 => s"Int"
    case I64 => s"Long"
    case F32 => s"Float"
    case F64 => s"Double"
  }

  /**
    * Change the template file
    * @param file
    */
  def changeTemplate(file: String) = templateFile = file

  /**
    * Creates the string representing the scala function return based on the arity of the return object in WASM
    * @param t
    * @return
    */
  private def buildReturn(t: Vector[ValType]): String = {
    if (t.isEmpty)
      "Unit"
    else {
      if (t.length == 1)
        getScalaType(t.last)
      else
        t.map(getScalaType).mkString("(", ",", ")")
    }
  }

  /**
    * Generates a context Map to feed the template rendering
    * @param imports
    * @return
    */
  def getContext(imports: Vector[Import]): Seq[Map[String, Any]] = {
    val sorted = imports
      .collect { case i: Import.Function => i } // Filter by function type
      .groupBy(t => t.moduleName) // Group by module
      .view
      .mapValues(_.toSet) // remove duplicated entries

    // Generating DTO
    val dto = sorted.keys.zipWithIndex
      .map(
        m =>
          Map(
            "module" -> m._1,
            "comma" -> (m._2 < sorted.keys.size - 1),
            "fields" -> sorted(m._1).toSeq.zipWithIndex
              .map(
                f => {
                  Map(
                    "name" -> f._1.fieldName,
                    "return" -> buildReturn(f._1.tpe.t),
                    "params" -> f._1.tpe.params.zipWithIndex.map(t => s"p${t._2}:${getScalaType(t._1)}").mkString(","),
                    "comma" -> (f._2 < sorted(m._1).size - 1)
                  )
                }
              )
          ))
      .toSeq

    dto
  }

  /**
    * Creates the trait to implement the WASM import functions
    * @param imports
    * @return
    */
  def generateImportText(imports: Vector[Import], className: String) = {

    val te = new TemplateEngine()
    te.boot()

    val result =
      te.layout(templateFile,
                Map(
                  "className" -> className,
                  "imports" -> getContext(imports)
                ))

    val file = Paths.get(s"GeneratedImport.scala")

    try {
      scalafmt.format(config, file, result)
    } catch {
      case x: Exception => {
        throw new RuntimeException(s"It seems that the generated file is not a valid scala file. ${x.getMessage}")
      }
    }
  }

  /**
    * Creates a new module including a trait object to implement the Imports
    * @param projectName
    * @param imports
    * @return
    */
  def createScalaProjectForImports(projectName: String,
                                   imports: Vector[Import],
                                   className: String = "GeneratedImport") = {
    s"mkdir -p $projectName".!!
    s"mkdir -p $projectName/src".!!

    val trait_ = generateImportText(imports, className)

    val printer = new PrintWriter(new File(s"$projectName/src/$className.scala"))
    printer.print(trait_)
    printer.close()
  }
}

object ImportGenerator {

  def make(): ImportGenerator =
    new ImportGenerator()

}
