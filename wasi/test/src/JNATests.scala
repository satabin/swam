package swam
package wasi

import java.{io, util}
import java.lang.reflect.{Field, Modifier}
import java.nio.file.Paths

import utest.{TestSuite, Tests, test}
import runtime._
import swam.test.util._
import utest._
import better.files._
import cats.effect._
import com.sun.jna.{Native, Structure}
import fs2.io.file
import swam.impl.JNA.LibCWrapper
import swam.impl.JNA.helper.LibCHelper
import swam.impl.JNA.impl.PosixFactory
import swam.impl.JNA.impl.macos.MacOSFileStat
import swam.text.parser
import swam.witx.WitxParser
import swam.witx.parser.{ImportContext, TypesParser}
import swam.witx.unresolved._
import swam.witx
import swam.witx.traverser.{ModuleInterfaceTraverser, TypesTraverser}

import scala.concurrent.ExecutionContext

object JNATest extends TestSuite {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def runTest() = {

    val st = PosixFactory()

    val stat = st.fstat(1)

    println(stat.mode())
  }

  val tests = Tests {
    "JNA_test" - runTest()
  }

}
