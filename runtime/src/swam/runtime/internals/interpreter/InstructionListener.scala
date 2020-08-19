package swam
package runtime
package internals
package interpreter

import swam.runtime.internals.instance.FunctionInstance

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
trait InstructionListener[F[_]] {

  val wasiCheck : Boolean

  def init(inner: InstructionWrapper[F], index:Int, functionName: Option[String]): Unit
  def before(inner: InstructionWrapper[F], index:Int, frame: Frame[F]): Unit
  def after(inner: InstructionWrapper[F],  index:Int, frame: Frame[F], functionName:Option[String], result: Continuation[F]): Continuation[F]
}
