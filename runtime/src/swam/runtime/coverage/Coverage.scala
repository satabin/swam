package swam
package runtime
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

import cats.implicits._
import cats.effect._
import cats.effect.IO

import java.nio.file._
import java.util.logging._
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import java.nio.file.Path
import java.io.File

class ModuleCoverageInfo{
  private var _methodName: String = ""
  private var _coveredInst: Long = 0
  private var _totalInst: Long = 0

  def getMethodName = this._methodName
  def setMethodName(str:String) : Unit = {
    this._methodName = str
  }

  def getCoveredInst = this._coveredInst
  def setCoveredInst(ci:Long) : Unit = {
    this._coveredInst = ci
  }

  def getTotalInst = this._totalInst
  def setTotalInst(ti:Long) : Unit = {
    this._totalInst = ti
  }
}

/** Factory for [[swam.runtime.coverage.CoverageType]] instances. */
object CoverageType{
  /** Creates a person with a given name and age.
   *
   *  @param watOrWasm the filename with absolute path
   *  @param instance the compiled webassembly functions in the Instance[F] form.
   */
  def instCoverage(watOrWasm: Path,instance : Instance[IO]) : Unit = {
    /**
     * Todo code for printing the code coverage from swam-cli in below mentioned formats.
     * 1) console
     * 2) csv
     * 3) html
     * val logger = new PrintWriter(new BufferedWriter(new FileWriter("coverage.csv", true)))
    logger.println(s"\u2500"*25)
    logger.println("Instruction Coverage- " + watOrWasm.getFileName())
    logger.println(s"\u2500"*25)
    val logger = new PrintWriter(new BufferedWriter(new FileWriter("coverage.csv")))
     */
    val replaceFile = watOrWasm.getFileName.toString().replaceAll("\\.\\w+", "")
    val logger = new PrintWriter(new BufferedWriter(new FileWriter(replaceFile +".ic.csv")))
    logger.println("Method Name, Covered Instruction, Total Instruction") 
    instance.module.functions.foreach(f => {
      val count = f.code
        .collect {
          case x: Coverage[IO] => {
            x
          }
        }
        .count(x => x.hitCount > 0)
      //logger.println(s"${f.idx} -> ${100.0 * count / f.code.length}%")
      instance.module.names.flatMap(_.subsections.collectFirstSome {
            case FunctionNames(names) =>
              //println(names.get(f.idx))
              names.get(f.idx) match {
                //case Some(x) => logger.println(s"${x} -----> ${100.0 * count / f.code.length}%")  
                case Some(x) => logger.println(s"${x}, ${count}, ${f.code.length}")  
                //case _ => logger.println(s"Function name does not exist -----> ${100.0 * count / f.code.length}%")
                case _ => logger.println(s"N/A, ${count} , ${f.code.length}")
              }
              names.get(f.idx)
            case _ =>
              None
          })
    })
    logger.close()
  }

  //logger
  //logger type case classes
  //instance class

  def instCoverage_updated(dir: Any, watOrWasm: Path,instance : Instance[IO], logOrNot: Boolean) = {
    val list = BuildCoverage(instance)
    if(logOrNot)
      LogCoverage(dir, watOrWasm, list)
  }

  def LogCoverage(dir: Any, watOrWasm: Path, list: ListBuffer[ModuleCoverageInfo]) = {
    val fn = watOrWasm.getFileName.toString
    val index = fn.lastIndexOf('.')
    val mn : String = fn.substring(0, index)
    val logger = dir match {
      case Some(x) => new PrintWriter(new BufferedWriter(new FileWriter(x.toString + "/" + mn +".ic.csv")))
      case _ => new PrintWriter(new BufferedWriter(new FileWriter(mn +".ic.csv")))
    }

    logger.println("Method Name, Covered Instruction, Total Instruction")
    list.map(f => logger.println(s"${f.getMethodName}, ${f.getCoveredInst}, ${f.getTotalInst}"))
    logger.close()
  }

  def BuildCoverage(instance : Instance[IO]) : ListBuffer[ModuleCoverageInfo]= {
    val covList = new ListBuffer[ModuleCoverageInfo]();
    println(instance.module.functions.foreach(f=>println(f)))
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
              val xv = new ModuleCoverageInfo()
              xv.setMethodName(x.toString)
              xv.setCoveredInst(count)
              xv.setTotalInst(f.code.length)
              covList += xv
            }
            case _ => {
              val xv = new ModuleCoverageInfo()
              xv.setMethodName("N/A")
              xv.setCoveredInst(count)
              xv.setTotalInst(f.code.length)
              covList += xv
            }
          }
          names.get(f.idx)
        case _ =>
          None
      })
    })
    //logger.close()
    covList
  }
}
