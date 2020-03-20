package swam
package generator

import utest.{TestSuite, Tests, test}

import runtime._
import swam.test.util._
import utest._
import better.files._
import cats.effect._
import fs2.io.file
import swam.text.parser
import swam.witx.WitxParser
import swam.witx.parser.{ImportContext, TypesParser}
import swam.witx.unresolved.{FunctionExport, ImportDeclaration}
import swam.witx

import scala.concurrent.ExecutionContext
object WitParser extends TestSuite {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def runParse() = {}

  val tests = Tests {
    "console_tracer" - runParse()

  }

}
