package swam
package witx

import java.nio.file.Path

import fastparse._
import swam.text.{ParserException, readFile}
import swam.witx.parser.{ImportContext, ModuleParser, TypesParser}
import swam.witx.unresolved.{BaseWitxType, ModuleInterface}
import cats.effect._
import cats.implicits._

/**
  * @author Javier Cabrera-Arteaga on 2020-03-20
  */
class WitxParser[F[_]](implicit val F: Effect[F]) {

  def parseModuleInterface(file: Path, blocker: Blocker, ctx: ImportContext[F], chunkSize: Int = 1024)(
      implicit cs: ContextShift[F]): F[ModuleInterface] =
    for {
      input <- readFile(file, blocker, chunkSize)
      interface <- parseModuleString(input, ctx)
    } yield interface

  private[swam] def parseModuleString(input: String, ctx: ImportContext[F]): F[ModuleInterface] =
    F.liftIO {
      IO(fastparse.parse(input, ModuleParser[F](ctx).file(_))).flatMap {
        case Parsed.Success(m, _)          => IO.pure(m)
        case f @ Parsed.Failure(_, idx, _) => IO.raiseError(new ParserException(f.msg, idx))
      }
    }

  def parseTypes(file: Path, blocker: Blocker, chunkSize: Int = 1024)(
      implicit cs: ContextShift[F]): F[Map[String, BaseWitxType]] =
    for {
      input <- readFile(file, blocker, chunkSize)
      interface <- parseTypesString(input)
    } yield interface

  private[swam] def parseTypesString(input: String): F[Map[String, BaseWitxType]] =
    F.liftIO {
      IO(fastparse.parse(input, TypesParser.file(_))).flatMap {
        case Parsed.Success(m, _)          => IO.pure(m)
        case f @ Parsed.Failure(_, idx, _) => IO.raiseError(new ParserException(f.msg, idx))
      }
    }

}

object WitxParser {
  def apply[F[_]](implicit F: Effect[F]): WitxParser[F] = new WitxParser[F]()
}
