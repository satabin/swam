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

object CoverageReporter {

  def buildCoverage(listener: CoverageListener[IO]): List[ModuleCoverageInfo] = {

    val covList = listener.coverageMap
      .groupBy {
        case (_, (name, _)) => name
      }
      .toList
      .map {
        case (name, map) => ModuleCoverageInfo(name, map.count(t => t._2._2 > 0), map.size)
      }

    covList
  }

  private def logCoverage(dir: Any, watOrWasm: Path, list: List[ModuleCoverageInfo]): Unit = {

    implicit val modEncoder: HeaderEncoder[ModuleCoverageInfo] =
      HeaderEncoder.caseEncoder("Method Name", "Covered Block", "Total Blocks")(ModuleCoverageInfo.unapply _)

    val fn = watOrWasm.getFileName.toString
    val index = if (fn.lastIndexOf('.') > -1) fn.lastIndexOf('.') else fn.length
    val mn: String = fn.substring(0, index)
    val logger: String = dir match {
      case Some(x) => x.toString + "/" + mn + ".ic.csv"
      case _       => mn + ".ic.csv"
    }

    val out = new File(logger)

    println(out)
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
  def instCoverage(dir: Any, watOrWasm: Path, instance: CoverageListener[IO], logOrNot: Boolean) = {
    val list = buildCoverage(instance)
    //if (logOrNot)
    logCoverage(dir, watOrWasm, list)
  }
}
