package swam
package optin
package coverage

/**
  * Provides classes for dealing with Coverage information of WebAssembly information.
  *
  *  ==Overview==
  *
  * */
import kantan.csv._
import kantan.csv.ops._

import scala.collection.mutable.ListBuffer

import swam.runtime._
import runtime.internals.interpreter._
import swam.runtime.internals.compiler.CompiledFunction
import swam.binary.custom.FunctionNames

import cats.implicits._
import cats.effect._
import cats.effect.IO

import java.nio.file._
import java.util.logging._
import java.nio.file.Path
import java.io.File

case class ModuleCoverageInfo(methodName: String, coveredInst: Long, totalInst: Long)

object CoverageType {

  def buildCoverage(instance: Instance[IO]): Unit = {}

  private def logCoverage(dir: Any, watOrWasm: Path, list: ListBuffer[ModuleCoverageInfo]): Unit = {

    implicit val modEncoder: HeaderEncoder[ModuleCoverageInfo] =
      HeaderEncoder.caseEncoder("Method Name", "Covered Instruction", "Total Instruction")(ModuleCoverageInfo.unapply _)

    val fn = watOrWasm.getFileName.toString
    val index = fn.lastIndexOf('.')
    val mn: String = fn.substring(0, index)
    val logger: String = dir match {
      case Some(x) => x.toString + "/" + mn + ".ic.csv"
      case _       => mn + ".ic.csv"
    }

    val out = new File(logger)

    val writer = out.asCsvWriter[ModuleCoverageInfo](rfc.withHeader)

    list.map(f => {
      val ModuleCoverageInfo(m, c, t) = f
      writer.write(ModuleCoverageInfo(m, c, t))
    })

    writer.close()
  }

  /** Creates a person with a given name and age.
    *  @param watOrWasm the filename with absolute path
    *  @param instance the compiled webassembly functions in the Instance[F] form.
    */
  def instCoverage(dir: Any, watOrWasm: Path, instance: Instance[IO], logOrNot: Boolean) = {
    //val list = buildCoverage(instance)

    {}
  }
}
