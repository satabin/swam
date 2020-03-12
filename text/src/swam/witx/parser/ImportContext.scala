package swam.witx.parser

import java.io.File
import java.nio.file.{Files, Path, Paths}

import cats.effect._
import fastparse.Parsed
import swam.text.{ParserException, readFile}
import swam.witx.WitxParser
import swam.witx.unresolved.{AliasType, BaseWitxType, ModuleInterface}

import scala.concurrent.ExecutionContext

/**
  *@author Javier Cabrera-Arteaga on 2020-03-18
  */
class ImportContext[F[_]](implicit val F: Effect[F]) {

  // TODO FIX !
  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def load(path: String) = {
    Blocker[IO]
      .use { blocker =>
        WitxParser[IO].parseTypes(Paths.get(
                                    s"generator/resources/wasi_witx/$path"
                                  ),
                                  blocker)
      }
      .unsafeRunSync()
  }

  def getInPath(path: String, includes: String*): String = {
    if (Files.exists(Paths.get(path)))
      path
    else
      System
        .getenv("PATH")
        .split(":")
        .concat(includes)
        .filter(t => Files.exists(Paths.get(s"$t/$path")))(0)
  }

}

object ImportContext {

  def apply[F[_]: Effect](): ImportContext[F] = new ImportContext[F]()
}
