package swam
package stdlib
package impl

import cats.effect.IO
import swam.stdlib.GeneratedImports

/**
  * @author Javier Cabrera-Arteaga on 2020-03-12
  */
class Imports extends GeneratedImports {
  def envSrand(p0: Int): IO[Unit] = IO {}

  def envTime(p0: Int): IO[Int] = IO { 1 }

  def envRand(): IO[Int] = IO { 1 }
}
