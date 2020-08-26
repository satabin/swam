package swam
package runtime
package internals
package interpreter

import swam.runtime.internals.interpreter.{Continuation, Frame, InstructionWrapper}

trait InstructionListener[F[_]] {

  val wasiCheck: Boolean

  def init(inner: InstructionWrapper[F], functionName: Option[String]): Unit
  def before(inner: InstructionWrapper[F], frame: Frame[F]): Unit
  def after(inner: InstructionWrapper[F],
            frame: Frame[F],
            functionName: Option[String],
            result: Continuation[F]): Continuation[F]
}
