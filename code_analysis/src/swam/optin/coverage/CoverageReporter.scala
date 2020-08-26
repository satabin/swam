package swam
package code_analysis
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
import cats.effect.{Async, ContextShift, IO, Blocker, Sync}

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import java.io._
import swam.runtime.internals.instance._

import scala.concurrent.ExecutionContext

//import scala.util.Random

case class ModulePathCoverage(methodName: String, src: Int, dest: Int, hitCount: Long)

case class ModuleCoverageInfo(methodName: String, coveredInst: Long, totalInst: Long)

case class ModuleShowMap(methodName: String, instIndex: Long, hitCount: Long)

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
        case (_, (name, _)) => name
      }
      .toList
      .map {
        case (name, map) => ModuleCoverageInfo(name, map.count(t => t._2._2 > 0), map.size)
      }

    covList
  }

  /**
    * Function returns the afl-showmap in the form of a string
    * @param listener
    */
  def buildShowMap(listener: CoverageListener[IO]): List[ModuleShowMap] = {
    val sm = new ListBuffer[ModuleShowMap]()
    listener.coverageMap foreach {
      case ((index), value) => sm += ModuleShowMap(value._1, index, value._2)
    }
    sm.toList
  }

  private def writeToReport(report: String, reportName: String, t: String, extension: String) = {
    Blocker[IO]
      .use { blocker =>
        for {
          _ <- io.file.createDirectories[IO](blocker, Paths.get(s"$report")) // Creates the module structure
          _ <- io.file.deleteIfExists[IO](blocker, Paths.get(s"$report/$reportName.$extension"))
          _ <- fs2
            .Stream(t.toString)
            .through(text.utf8Encode)
            .through(io.file
              .writeAll[IO](Paths.get(s"$report/$reportName.$extension"), blocker, Seq(StandardOpenOption.CREATE)))
            .compile
            .drain
        } yield ()
      }
      .unsafeRunSync()
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
  private def logCoverage(
      dir: Path,
      watOrWasm: Path,
      list: List[ModuleCoverageInfo],
      showMap: List[ModuleShowMap]
  ) = {
    //println("This is dir " + dir)

    val fn = watOrWasm.getFileName.toString
    val index = fn.lastIndexOf('.')
    val mn: String = fn.substring(0, index)

    val report = dir.toString + "/cov_results/" + mn + "_covreport"
    val reportName = mn

    /*Report*/
    val t = new StringBuilder("")
    t.append(s"Method Name, Covered Block, Total Block\n")

    /*Showmap*/
    val t1 = new StringBuilder("")
    t1.append(s"s.no, method, block_index, hitcount\n")
    list foreach { case ModuleCoverageInfo(m, c, tc) => t.append(s"$m,$c,$tc\n") }
    writeToReport(report, reportName, t.toString, "ic.csv")
    showMap.zipWithIndex.foreach {
      case (ModuleShowMap(m, i, h), index) =>
        t1.append(s"$index, $m,  $i, $h\n")
        writeToReport(report, reportName, t1.toString, "showmap.csv")
    }
    list foreach { case ModuleCoverageInfo(m, c, tc) => t.append(s"$m,$c,$tc\n") }
    writeToReport(report, reportName, t.toString, "ic.csv")
    showMap foreach { case ModuleShowMap(m, i, h) => t1.append(s"$index, $m, $i, $h\n") }
    writeToReport(report, reportName, t1.toString, "showmap.csv")
  }

  def checkCoverageMap(dir: Path,
                       watOrWasm: Path,
                       report: List[ModuleCoverageInfo],
                       showmap: List[ModuleShowMap],
                       instance: CoverageListener[IO]) = {
    if (!instance.coverageMap.isEmpty)
      logCoverage(dir, watOrWasm, report, showmap)
    else {
      import scala.Console.{RESET, YELLOW}
      println(s"${RESET}${YELLOW}No Coverage${RESET}")
      println(s"${RESET}${YELLOW}Reason being : ${RESET}")
      println(s"${RESET}${YELLOW}1) There could be no method in the Wasm module.${RESET}")
      println(
        s"${RESET}${YELLOW}2) Or there is no funtions matching the filter options given in --func-filter.${RESET}")
      println(s"${RESET}${YELLOW}   Please adjust the filter and try again.${RESET}")
    }
  }

  /**
    * Interface to prints Instruction Coverage report and afl show-map from the cli tool
    * @param dir
    * @param watOrWasm
    * @param instance
    */
  def instCoverage(dir: Path, watOrWasm: Path, instance: CoverageListener[IO]) = {

    val report = buildCoverage(instance)
    val showmap = buildShowMap(instance)
    checkCoverageMap(dir, watOrWasm, report, showmap, instance)

  }

  /** Creates a person with a given name and age.
    *  @param watOrWasm the filename with absolute path
    *  @param instance the compiled webassembly functions in the Instance[F] form.
    */
  def blockCoverage(dir: Path, watOrWasm: Path, instance: CoverageListener[IO]) = {
    val list = buildCoverage(instance)
    val showmap = buildShowMap(instance)
    //if (logOrNot)
    logCoverage(dir, watOrWasm, list, showmap)
  }
}
