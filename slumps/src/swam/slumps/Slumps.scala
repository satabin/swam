/*
 * Slumps team
 */

package swam
package slumps

import config._

import cats.implicits._
import cats.effect._

import fs2._

import pureconfig._
import pureconfig.generic.auto._
import pureconfig.module.squants._
import pureconfig.module.catseffect._

import scala.language.higherKinds

import java.nio.file.Path

/** Main class for slumps operations
  */
class Slumps private (val conf: SlumpsConfiguration) {

}

object Slumps {

  def apply[F[_]: Effect](): F[Slumps] =
    for {
      conf <- ConfigSource.default.at("swam.slumps").loadF[F, SlumpsConfiguration]
    } yield new Slumps(conf)

  def apply[F[_]: Effect](conf: SlumpsConfiguration): Slumps = {
    new Slumps(conf)
  }

}
