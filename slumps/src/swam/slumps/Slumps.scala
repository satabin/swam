/*
 * Slumps team
 */

package swam
package slumps

import config._
import binary._
import validation._
import syntax._

import cats.implicits._
import cats.effect._


import pureconfig._
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

import scala.language.higherKinds

import java.nio.file.Path

/** Main class for slumps operations
  */
class Slumps[F[_]: Effect] private (val conf: SlumpsConfiguration, val validator: Validator[F]) 
    extends ModuleLoader[F] {

    private val binaryParser = new ModuleParser[F](validator)

    def processPath(path: Path, blocker: Blocker, chunkSize: Int = 1024)(implicit cs: ContextShift[F]):Int
    = processModule(
        binaryParser.parse(
            super.sections(path, blocker, chunkSize)
        ))
        
    def processModule(module: F[Module]):Int = 0

}


object Slumps {

    def apply[F[_]: Effect](): F[Slumps[F]] =
      for {
        validator <- Validator[F]
        conf <- ConfigSource.default.at("swam.runtime").loadF[F, SlumpsConfiguration]
      } yield new Slumps[F](conf, validator)
  
    def apply[F[_]: Effect](conf: SlumpsConfiguration, validator: Validator[F]): Slumps[F] = {
      new Slumps[F](conf, validator)
    }
  
  }
  
