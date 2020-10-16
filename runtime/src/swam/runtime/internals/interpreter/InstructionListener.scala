package swam
package runtime
package internals
package interpreter

import fs2.Stream
import swam.runtime.internals.interpreter.{Continuation, Frame, InstructionWrapper}
import swam.syntax.Section

trait InstructionListener[F[_]] {

  val wasiCheck: Boolean

  def init(inner: InstructionWrapper[F], functionName: Option[String]): Unit
  def before(inner: InstructionWrapper[F], frame: Frame[F]): Unit
  def after(inner: InstructionWrapper[F],
            frame: Frame[F],
            functionName: Option[String],
            result: Continuation[F]): Continuation[F]

  def instrument(sections: Stream[F, Section]): Stream[F, Section]

}
