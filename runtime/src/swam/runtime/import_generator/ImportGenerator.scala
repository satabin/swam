package swam.runtime.import_generator

import java.io.{File, PrintWriter}

import swam.{FuncType, ValType}
import swam.ValType.{F32, F64, I32, I64}
import swam.runtime.Import
import scala.reflect.runtime.universe._

import sys.process._

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  */
/**
  * Generates the scala code with the template to use as Import in the WASM execution
  */
class ImportGenerator() {

  val f = "IO"

  /**
    * Map WASM to scala primitive types
    * @param v
    * @return
    */
  def getScalaType(v: ValType): String = v match {
    case I32 => s"Int"
    case I64 => s"Long"
    case F32 => s"Float"
    case F64 => s"Double"
  }

  /**
    * Creates the paramaters chain for the function signature in scala
    * @param index
    * @param tpr
    * @return
    */
  def buildParameter(index: Int, tpr: ValType) =
    s"p$index:${getScalaType(tpr)}"

  /**
    * Creates the string representing the scala function return based on the arity of the return object in WASM
    * @param t
    * @return
    */
  def buildReturn(t: Vector[ValType]) = {
    if (t.isEmpty)
      "Unit"
    else {
      if (t.length == 1)
        getScalaType(t.last)
      else
        t.map(getScalaType(_).mkString("(", ",", ")")
    }
  }

  var registeredFunctions: Set[String] = Set[String]()

  /**
    * Creates the function trait template
    * @param moduleName
    * @param fieldName
    * @param func
    * @return
    */
  def buildFunctionTemplate(moduleName: String, fieldName: String, func: FuncType): String = {
    val name = s"${moduleName}_$fieldName"

    s"def $name(${func.params
      .zipWithIndex
      .map {
        case (p, idx) => buildParameter(idx, p)
      }
      .mkString(",")}): $f[${buildReturn(func.t)}]"
  }

  /**
    * Creates the trait to implement the WASM import functions
    * @param imports
    * @return
    */
  def generateImportText(imports: Vector[Import]) = {

    val sorted = imports.sortBy(f => f.moduleName)

    var impl =
      s"""
        |import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
        |import swam.runtime.formats._
        |import swam.runtime.formats.DefaultFormatters._
        |
        |
        |trait GeneratedImport {
        |  type AsIIO[T] = AsInterface[T, $f]
        |  type AsIsIO[T] = AsInstance[T, $f]
        |
        |""".stripMargin

    var import_ = ""

    var module = ""
    var functions = Seq[String]()

    sorted.foreach {
      case Import.Function(moduleName, fieldName, _, tpe) => {

        if (module.isEmpty || module != moduleName) {
          import_ +=
            s""""$moduleName" -> TCMap[String, AsIIO]("""

          if (module.isEmpty)
            module = moduleName

        }
        val name = s"${moduleName}_$fieldName"

        if (registeredFunctions contains name) {
          System.err.println(s"WARNING: '$fieldName' already defined. Skipping generation")
        } else {
          registeredFunctions += name
          impl +=
            s"""
                  | ${buildFunctionTemplate(moduleName, fieldName, tpe)}
                  |""".stripMargin

          functions = functions :+ s"""
                              |                  "$fieldName" -> ${moduleName}_$fieldName _ """.stripMargin
        }

        if (module != moduleName) {
          import_ += functions.mkString(",")
          functions = Seq[String]()
          import_ += ")"
          module = moduleName
        }
      }
    }
    import_ += functions.mkString(",")

    s"""
       | $impl
       |  def imports() = {
       |    Imports[${f}](
       |          TCMap[String, AsIsIO](
       |            $import_ ))
       |  )
       |  }
       | }
       |""".stripMargin
  }

  /**
    * Creates a new module including a trait object to implement the Imports
    * @param projectName
    * @param imports
    * @return
    */
  def createScalaProjectForImports(projectName: String, imports: Vector[Import]) = {
    s"mkdir -p $projectName".!!
    s"mkdir -p $projectName/src".!!

    val trait_ = generateImportText(imports)

    val printer = new PrintWriter(new File(s"$projectName/src/GeneratedImport.scala"))
    printer.print(trait_)
    printer.close()
  }
}

object ImportGenerator {

  def make(): ImportGenerator =
    new ImportGenerator()

}
