package impl

import cats.effect.IO
import swam.stdlib.GeneratedImports

/**
  * @author Javier Cabrera-Arteaga on 2020-03-12
  */
class Imports extends GeneratedImports {
  override def envSrand(p0: Int): IO[Unit] = IO {}

  override def envTime(p0: Int): IO[Int] = IO { 1 }

  override def envPrintf(p0: Int, p1: Int): IO[Int] = IO { 1 }

  override def envRand(): IO[Int] = IO { 1 }
}
