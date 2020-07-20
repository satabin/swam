package swam
package optin
package coverage

/**
  * Provides classes for dealing with Coverage information of WebAssembly information.
  *
  *  ==Overview==
  *
  * */
import scala.collection.mutable.ListBuffer

import swam.runtime._

import runtime.internals.interpreter._
import swam.runtime.internals.compiler.CompiledFunction
import swam.binary.custom.FunctionNames

import cats._
import fs2._
import fs2.io._
import cats.implicits._
import cats.effect.{Async, ContextShift,IO,Blocker, Sync}

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import java.io._
import swam.runtime.internals.instance._

import scala.concurrent.ExecutionContext

//import scala.util.Random

case class ModuleCoverageInfo(methodName: String, coveredInst: Long, totalInst: Long)

case class ModuleShowMap(methodName: String, inst: String,instIndex: Long, hitCount: Long)

object CoverageReporter {

/**
*Implicit for the executing the ContectShift for fs2.io without extending the object with IOApp
*/
implicit val cs = IO.contextShift(ExecutionContext.global)

/**
   * Function returns the Coverage in the form of a List.
   * @param listener
   * @return returns List of Coverage information in the form of Method name, Covered Instructions and Total instructions
   */
   def buildCoverage(listener: CoverageListener[IO]): List[ModuleCoverageInfo] = {

    val covList = listener.coverageMap
      .groupBy {
        case ((_,_), (name, _, _)) => name
      }
      .toList
      .map {
        case (name, map) => ModuleCoverageInfo(name, map.count(t => t._2._3 > 0), map.size)
      }
    /**
     * Print coverage for checking or testing
     * */
    //println(listener.coverageMap)
    covList
  }

  /**
   * Function returns the afl-showmap in the form of a string
   * @param listener
   */
  def buildShowMap(listener: CoverageListener[IO]) : List[ModuleShowMap] = {
    val sm = new ListBuffer[ModuleShowMap]()
    listener.coverageMap foreach {case ((index), value) => sm += ModuleShowMap(index._2,value._2.toString, index._1, value._3)}
    //println(sm)
    sm.toList
  }

  /**
   * Function prints 2 things 
   * 1. Coverage Report to a csv file. ("Method Name", "Covered Instruction", "Total Instruction")
   * 2. Showmap to txt file.(TODO: Format of this file)
   * @param dir
   * @param watOrWasm
   * @param list
   * @param showMap
   */
  private def logCoverage[F[_]: Sync: ContextShift](
    dir: Path, 
    watOrWasm: Path,
    list: List[ModuleCoverageInfo], 
    showMap: List[ModuleShowMap],
    )(implicit F: Sync[F]) = {
    //println("This is dir " + dir)
    val t = new StringBuilder("")
    val t1 = new StringBuilder("")
    t.append(s"Method Name, Covered Instruction, Total Instruction\n")
    list foreach {case ModuleCoverageInfo(m,c,tc) => t.append(s"$m,$c,$tc\n")}
    t1.append(s"[")
    showMap foreach {case ModuleShowMap(m,in,i,h) => t1.append(s"\n\t{\n\t\tmethod : $m ,\n\t\tinstruction : $in ,\n\t\tinstruction_index : $i ,\n\t\tHitCount : $h\n\t},\n")}
    t1.append(s"]")
    val fn = watOrWasm.getFileName.toString
    val index = fn.lastIndexOf('.')
    val mn: String = fn.substring(0, index)

    val report = dir.toString + "/cov_results/" + mn + "_covreport" 

    val reportName = mn
    //println(s"$report")
    /*val prn = */Blocker[IO]
      .use { blocker =>
        for {
          _ <- io.file.createDirectories[IO](blocker, Paths.get(s"$report")) // Creates the module structure
          _ <- io.file.deleteIfExists[IO](blocker, Paths.get(s"$report/$reportName.ic.csv"))
          _ <- fs2
          .Stream(t.toString)
          .through(text.utf8Encode)
          .through(io.file
            .writeAll[IO](Paths.get(s"$report/$reportName.ic.csv"), blocker, Seq(StandardOpenOption.CREATE)))
          .compile
          .drain
          _ <- io.file.deleteIfExists[IO](blocker, Paths.get(s"$report/$reportName.showmap.txt"))
          _ <- fs2.
          Stream(t1.toString)
          .through(text.utf8Encode)
          .through(io.file
            .writeAll[IO](Paths.get(s"$report/$reportName.showmap.txt"), blocker, Seq(StandardOpenOption.CREATE)))
          .compile
          .drain
        } yield ()
      }.unsafeRunSync()

      //println(prn)
  }

  /**
   * Interface to prints Instruction Coverage report and afl show-map from the cli tool
   * @param dir
   * @param watOrWasm
   * @param instance
   * @param logOrNot
   */
  def instCoverage(dir: Path, watOrWasm: Path, instance: CoverageListener[IO]) = {

    val report = buildCoverage(instance)

    val showmap = buildShowMap(instance)

    logCoverage[IO](dir, watOrWasm, report, showmap)
    //if(fil){

      //val def_undef_func:Set[String] = filter.WasiFilter.readWasiFile()
      //val filter_report = filter.WasiFilter.filterCoverageReport(def_undef_func, report.toList)
      //println(filter_report.toList.toString)
      //val filter_showmap = filter.WasiFilter.filterCoverageShowMap(def_undef_func,showmap.toList)
      //logCoverage[IO](dir, watOrWasm, filter_report.toList, filter_showmap.toList)

    //}

    //else{

      

    //}

  }

}
