package swam
package wasi

import java.io
import java.nio.file.Paths

import utest.{TestSuite, Tests, test}
import runtime._
import swam.test.util._
import utest._
import better.files._
import cats.effect._
import com.sun.jna.Native
import fs2.io.file
import impl.FileStat
import swam.text.parser
import swam.wasi.libc
import swam.witx.WitxParser
import swam.witx.parser.{ImportContext, TypesParser}
import swam.witx.unresolved._
import swam.witx
import swam.witx.traverser.{ModuleInterfaceTraverser, TypesTraverser}

import scala.concurrent.ExecutionContext

object JNATest extends TestSuite {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def runTest() = {

    val f = new FileStat("/Users/javierca/Documents/Develop/slumps/crow/out/Bitwise_IO/Bitwise_IO.wasm")

    println(f.dev, f.ino, f.mode, f.nlink, f.size, f.isFile(), f.isDirectory())

  }

  val tests = Tests {
    "JNA_test" - runTest()
  }

}
