package swam
package optin
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener, InstructionWrapper}
import cats.effect.{Async, ContextShift}
import cats._
import java.io._

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
class CoverageListener[F[_]: Async](wasi: Boolean) extends InstructionListener[F] {
  var coverageMap = Map[Int, (String, Int)]()

  override val wasiCheck: Boolean = wasi

  override def before(inner: InstructionWrapper[F], frame: Frame[F]): Unit = {}

  override def after(inner: InstructionWrapper[F],
                     frame: Frame[F],
                     functionName: Option[String],
                     result: Continuation[F]): Continuation[F] = {
    val fn: String = functionName.getOrElse("N/A").toString
    val count: (String, Int) = coverageMap.getOrElse(inner.id, (fn, 0))
    coverageMap = coverageMap.updated(inner.id, (fn, count._2 + 1))
    result
  }

  override def init(inner: InstructionWrapper[F], functionName: Option[String]): Unit = {
    val fn = functionName.getOrElse("N/A").toString
    coverageMap = coverageMap.updated(inner.id, (fn, 0))
  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](wasi: Boolean): CoverageListener[F] = {

    new CoverageListener[F](wasi)

  }
}
