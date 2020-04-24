package swam
package generator

import java.nio.file.{Paths, StandardOpenOption}

import swam.ValType.{F32, F64, I32, I64}
import swam.runtime.Import

import scala.reflect.runtime.universe._
import org.scalafmt.interfaces.Scalafmt
import org.fusesource.scalate.TemplateEngine
import cats.implicits._
import cats.effect._
import fs2.{text, _}

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  * Generates the scala code with the template to use as Import in the WASM execution
  */
class ImportGenerator[F[_]: Sync: ContextShift] {

  val f = "IO"
  val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
  val config = Paths.get(".scalafmt.conf")
  val defaultTemplate = getClass.getClassLoader.getResource("import_template.mustache").getFile

  /**
    * Map WASM to scala primitive types
    * @param v
    * @return
    */
  private def getScalaType(v: ValType): String = v match {
    case I32 => "Int"
    case I64 => "Long"
    case F32 => "Float"
    case F64 => "Double"
  }

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
    sorted.zipWithIndex.map {
      case ((moduleName, functions), index) =>
        Map(
          "module" -> moduleName,
          "comma" -> (index < sorted.keys.size - 1),
          "fields" -> functions.toSeq.zipWithIndex
            .map {
              case (field, fieldIndex) => {
                Map(
                  "name" -> field.fieldName,
                  "nameCapital" -> field.fieldName.capitalize,
                  "return" -> buildReturn(field.tpe.t),
                  "params" -> field.tpe.params.zipWithIndex
                    .map { case (type_, typeIndex) => s"p${typeIndex}:${getScalaType(type_)}" }
                    .mkString(","),
                  "comma" -> (fieldIndex < functions.size - 1)
                )
              }
            }
        )
    }.toSeq
  }

  /**
    * Creates the trait to implement the WASM import functions
    * @param imports
    * @return
    */
  def generateImportText(imports: Vector[Import], className: String, templateFile: String = defaultTemplate) = {

    val te = new TemplateEngine()
    te.boot()

    val result =
      te.layout(templateFile,
                Map(
                  "className" -> className,
                  "imports" -> getContext(imports)
                ))

    val file = Paths.get("GeneratedImport.scala")

    scalafmt.format(config, file, result)
  }

  def writeToFile(t: String, projectName: String, className: String) = {

    Blocker[F]
      .use { blocker =>
        for {
          _ <- io.file.createDirectories[F](blocker, Paths.get(s"$projectName/src")) // Creates the module structure
          _ <- fs2
            .Stream(t)
            .through(text.utf8Encode)
            .through(io.file
              .writeAll[F](Paths.get(s"$projectName/src/$className.scala"), blocker, Seq(StandardOpenOption.CREATE)))
            .compile
            .drain
        } yield ()

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
                                   className: String = "GeneratedImport",
                                   template: String = defaultTemplate) = {

    val trait_ = generateImportText(imports, className, template)

    writeToFile(trait_, projectName, className)
  }
}

object ImportGenerator {

  def apply[F[_]: Sync: ContextShift]: ImportGenerator[F] =
    new ImportGenerator[F]

}
