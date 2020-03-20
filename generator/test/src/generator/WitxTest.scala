package swam
package generator

import java.io
import java.nio.file.Paths

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

  def runParse() = {
    val wasi_snaphot = Paths.get("text/resources/wasi_witx/wasi_snapshot_preview1.witx")

    val parser = WitxParser[IO]
    val ctx = ImportContext[IO]()

    Blocker[IO]
      .use(blocker => {
        for {
          instruction <- parser.parseModuleInterface(wasi_snaphot, blocker, ctx)
          _ <- IO(println(instruction))
        } yield ()
      })
      .unsafeRunSync()
  }

  val tests = Tests {
    "parsing_witx" - runParse()
  }

}
