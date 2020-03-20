package swam
package witx

import java.nio.file.Path
import cats.effect.{Blocker, ContextShift, Effect, IO}
import fastparse._
import swam.text.{ParserException, readFile}
import swam.witx.parser.{ImportContext, ModuleParser}
import swam.witx.unresolved.ModuleInterface
import cats.effect._
import cats.implicits._

/**
  * @author Javier Cabrera-Arteaga on 2020-03-20
  */
class WitxParser[F[_]](implicit val F: Effect[F]) {

  def parse(file: Path, blocker: Blocker, ctx: ImportContext, chunkSize: Int = 1024)(
      implicit cs: ContextShift[F]): F[ModuleInterface] =
    for {
      input <- readFile(file, blocker, chunkSize)
      interface <- parseString(input, ctx)
    } yield interface

  private[swam] def parseString(input: String, ctx: ImportContext): F[ModuleInterface] =
    F.liftIO {
      IO(fastparse.parse(input, ModuleParser(ctx).file(_))).flatMap {
        case Parsed.Success(m, _)          => IO.pure(m)
        case f @ Parsed.Failure(_, idx, _) => IO.raiseError(new ParserException(f.msg, idx))
      }
    }

}

object WitxParser {
  def apply[F[_]](implicit F: Effect[F]): WitxParser[F] = new WitxParser[F]()
}
