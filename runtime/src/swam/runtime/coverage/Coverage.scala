package swam
package runtime
package coverage

import swam.runtime._
import runtime.internals.interpreter._
import swam.runtime.internals.compiler.CompiledFunction

import cats.implicits._
import cats.effect._
import cats.effect.IO

import java.nio.file._
import java.util.logging._
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import java.nio.file.Path
import swam.binary.custom.FunctionNames


object CoverageType{
  def instCoverage(watOrWasm: Path,instance : Instance[IO]) : Unit = {
    //val logger = new PrintWriter(new BufferedWriter(new FileWriter("coverage.csv", true)))
    //logger.println(s"\u2500"*25)
    //logger.println("Instruction Coverage- " + watOrWasm.getFileName())
    //logger.println(s"\u2500"*25)

    //val logger = new PrintWriter(new BufferedWriter(new FileWriter("coverage.csv")))
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
}
