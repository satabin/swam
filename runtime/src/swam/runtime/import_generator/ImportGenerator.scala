package swam.runtime.import_generator

import swam.{FuncType, ValType}
import swam.ValType.{F32, F64, I32, I64}
import swam.runtime.Import

import scala.collection.mutable

/**
  * @author Javier Cabrera-Arteaga on 2020-03-06
  */
/**
  * Generates the scala code with the template to use as Import in the WASM execution
  */
class ImportGenerator(val F: String) {

  def getScalaType(v: ValType): String = v match {
    case I32 => s"Int"
    case I64 => s"Long"
    case F32 => s"Float"
    case F64 => s"Double"
  }

  def buildParameter(index: Int, tpr: ValType) =
    s"p${tpr match {
      case I32 => s"$index:${getScalaType(tpr)}"
      case I64 => s"$index:${getScalaType(tpr)}"
      case F32 => s"$index:${getScalaType(tpr)}"
      case F64 => s"$index:${getScalaType(tpr)}"
    }}"

  def buildReturn(t: Vector[ValType]) = {
    if (t.isEmpty)
      "Unit"
    else {
      if (t.length == 1)
        getScalaType(t.last)
      else
        s"(${t.map(getScalaType).mkString(",")})"
    }
  }

  var registeredFunctions: Set[String] = Set[String]()

  def buildFunctionTemplate(moduleName: String, fieldName: String, func: FuncType): String = {
    val name = s"${moduleName}_$fieldName"

    s"def $name(${func.params
      .zip(Seq.range(0, func.params.length))
      .map(t => {
        buildParameter(t._2, t._1)
      })
      .mkString(",")}): $F[${buildReturn(func.t)}]"
  }

  def generateImportFunnction(func: Import.Function): Unit = {}

  def generateImportText(imports: Vector[Import]) = {

    val sorted = imports.sortBy(f => f.moduleName)

    var impl =
      s"""
        |import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
        |
        |trait GeneratedImport {
        |  type AsIIO[T] = AsInterface[T, $F]
        |  type AsIsIO[T] = AsInstance[T, $F]
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
       |    Imports[${F}](
       |          TCMap[String, AsIsIO](
       |            $import_ ))
       |  )
       |  }
       | }
       |""".stripMargin
  }
}

object ImportGenerator {

  def make(F: String): ImportGenerator =
    new ImportGenerator(F)

}
