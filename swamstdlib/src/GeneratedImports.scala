package swam
package stdlib

import cats.effect.IO
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._

trait GeneratedImports {
  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  def envSrand(p0: Int): IO[Unit]
  def envTime(p0: Int): IO[Int]
  def envPrintf(p0: Int, p1: Int): IO[Int]
  def envRand(): IO[Int]
  def imports() = {
    Imports[IO](
      TCMap[String, AsIsIO](
        "env" -> TCMap[String, AsIIO]("srand" -> envSrand _,
                                      "time" -> envTime _,
                                      "printf" -> envPrintf _,
                                      "rand" -> envRand _))
    )
  }
}
