package swam
package optin
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener}
import cats.effect.{Async, ContextShift}
import cats._

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
class CoverageListener[F[_]: Async] extends InstructionListener[F] {

  var coverageMap = Map[AsmInst[F], Int]()

  override def before(inner: AsmInst[F], frame: Frame[F]): Unit = {}

  override def after(inner: AsmInst[F], frame: Frame[F], result: Continuation[F]): Continuation[F] = result

  override def init(inner: AsmInst[F]): Unit = {
    coverageMap = coverageMap.updated(inner, 0)
  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](): CoverageListener[F] = new CoverageListener[F]()
}
