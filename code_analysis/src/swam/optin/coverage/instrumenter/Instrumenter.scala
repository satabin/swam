package swam
package code_analysis
package coverage
package instrument

import enumeratum._

import cats.implicits._
import cats._
import cats.effect._

import fs2._
import swam.syntax.Section

/**
  * @author Javier Cabrera-Arteaga on 2020-10-16
  */
abstract class Instrumenter[F[_]](implicit F: MonadError[F, Throwable]) {
  def instrument(sections: Stream[F, Section]): Stream[F, Section]
}

sealed trait InstrumentationType extends EnumEntry with EnumEntry.Hyphencase
object InstrumentationType extends Enum[InstrumentationType] {

  case object JSCallback extends InstrumentationType
  case object InnerCallback extends InstrumentationType

  def values: IndexedSeq[InstrumentationType] = findValues
}
