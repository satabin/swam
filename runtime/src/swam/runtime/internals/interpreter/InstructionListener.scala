package swam
package runtime
package internals
package interpreter

import swam.runtime.internals.instance.FunctionInstance

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
trait InstructionListener[F[_]] {

  val wasiCheck: Boolean
  val buffer: Array[Byte]

  def init(inner: InstructionWrapper[F], functionName: Option[String]): Unit
  def before(inner: InstructionWrapper[F], frame: Frame[F]): Unit
  def after(inner: InstructionWrapper[F],
            frame: Frame[F],
            functionName: Option[String],
            result: Continuation[F]): Continuation[F]
}
