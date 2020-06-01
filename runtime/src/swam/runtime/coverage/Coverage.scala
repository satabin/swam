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

object CoverageType{
  def instCoverage(watOrWasm: Path,instance : Instance[IO]) : Unit = {
    val logger = new PrintWriter(new BufferedWriter(new FileWriter("coverage.log", true)))
    //new BufferedWriter(new FileWriter("myfile.txt", true))
    //val logger = new PrintWriter(watOrWasm + ".log",true)
    logger.println(s"\u2500"*25)
    logger.println("Instruction Coverage- " + watOrWasm.getFileName())
    logger.println(s"\u2500"*25)
    instance.module.functions.foreach(f => {
      val count = f.code
        .collect {
          case x: Coverage[IO] => {
            x
          }
        }
        .count(x => x.hitCount > 0)
      logger.println(s"${f.idx} -> ${100.0 * count / f.code.length}%")
    })
    logger.close()
  }
}
