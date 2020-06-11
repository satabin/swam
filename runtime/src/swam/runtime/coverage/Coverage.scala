package swam
package runtime
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

case class ModuleCoverageInfo(methodName: String, coveredInst : Long, totalInst : Long)  

/** Factory for [[swam.runtime.coverage.CoverageType]] instances. */
object CoverageType{

  def buildCoverage(instance : Instance[IO]) : ListBuffer[ModuleCoverageInfo]= {
    val covList = new ListBuffer[ModuleCoverageInfo]();
    //println(instance.module.functions.foreach(f=>println(f)))
    instance.module.functions.foreach(f => {
      val count = f.code
        .collect {
          case x: Coverage[IO] => {
            x
          }
        }
        .count(x => x.hitCount > 0)
      instance.module.names.flatMap(_.subsections.collectFirstSome {
        case FunctionNames(names) =>
          //println(names.get(f.idx))
          names.get(f.idx) match {
            case Some(x) => {
              val cc= ModuleCoverageInfo(x.toString, count, f.code.length)              
              covList += cc
            }
            case _ => {
              val cc= ModuleCoverageInfo("N/A", count, f.code.length)
              covList += cc
            }
          }
          names.get(f.idx)
        case _ =>
          None
      })
    })
    covList
  }

  
  private def logCoverage(dir: Any, watOrWasm: Path, list: ListBuffer[ModuleCoverageInfo]) : Unit = {

    implicit val modEncoder: HeaderEncoder[ModuleCoverageInfo] = 
      HeaderEncoder.caseEncoder("Method Name", "Covered Instruction", "Total Instruction")(ModuleCoverageInfo.unapply _)

    val fn = watOrWasm.getFileName.toString
    val index = fn.lastIndexOf('.')
    val mn : String = fn.substring(0, index)
    val logger:String = dir match {
      case Some(x) => x.toString + "/" + mn +".ic.csv"
      case _ => mn +".ic.csv"
    }

    val out = new File(logger)

    val writer = out.asCsvWriter[ModuleCoverageInfo](rfc.withHeader)

    /*for (l <- list) {
      val ModuleCoverageInfo(methodName, coveredInst, totalInst) = l
      logger.println(s"${methodName}, ${coveredInst}, ${totalInst}")  
    }*/

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
  def instCoverage(dir: Any, watOrWasm: Path,instance : Instance[IO], logOrNot: Boolean) = {
    val list = buildCoverage(instance)
    if(logOrNot)
      logCoverage(dir, watOrWasm, list)
  }
}