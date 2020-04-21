package swam
package generator

import java.io
import java.nio.file.Paths
import org.fusesource.scalate.TemplateEngine

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
import swam.witx.unresolved._
import swam.witx
import swam.witx.traverser.{ModuleInterfaceTraverser, TypesTraverser}

import scala.concurrent.ExecutionContext

class TypeMustacheWrapper(val inner: BaseWitxType) {
  val isEnum = inner.getClass.equals(classOf[EnumType])
  val isFlag = inner.getClass.equals(classOf[FlagsType])
  val isStruct = inner.getClass.equals(classOf[StructType])
}

object WitParser extends TestSuite {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def runParse() = {
    val wasi_snaphot = Paths.get("generator/resources/wasi_witx/wasi_snapshot_preview1.witx")

    val parser = WitxParser[IO]
    val ctx = ImportContext[IO]()

    val (types, interface) = Blocker[IO]
      .use(blocker => {
        for {
          (types, instruction) <- parser.parseModuleInterface(wasi_snaphot, blocker, ctx)
        } yield (types, instruction)
      })
      .unsafeRunSync()

    println(types)
  }

  def runGenerator() = {}

  val tests = Tests {
    "parsing_witx" - runParse()
    "generating_boilerplate_mustache" - runGenerator()
  }

}
