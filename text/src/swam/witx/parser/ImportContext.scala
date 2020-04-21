package swam.witx.parser

import java.io.File
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

import cats.effect._
import fastparse.Parsed
import swam.text.{ParserException, readFile}
import swam.witx.WitxParser
import swam.witx.unresolved.{AliasType, BaseWitxType, ModuleInterface}

import scala.concurrent.ExecutionContext

/**
  *@author Javier Cabrera-Arteaga on 2020-03-18
  */
class ImportContext[F[_]](val includes: List[String])(implicit val F: Effect[F]) {

  // TODO FIX !
  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def load(path: String) = {

    val paths = getInPath(path)

    if (paths.isEmpty)
      throw new NoSuchFileException(path)

    Blocker[IO]
      .use { blocker =>
        WitxParser[IO].parseTypes(paths.head, blocker)
      }
      .unsafeRunSync()
  }

  def getInPath(path: String): List[Path] = {
    if (Files.exists(Paths.get(path)))
      List(Paths.get(path)) // Current working directory
    else // else search in path
      System
        .getenv("PATH")
        .split(":")
        .concat(includes)
        .map(t => Paths.get(s"$t/$path"))
        .filter(t => Files.exists(t))
        .toList
  }

}

object ImportContext {

  def apply[F[_]: Effect](includes: List[String]): ImportContext[F] = new ImportContext[F](includes)
}
