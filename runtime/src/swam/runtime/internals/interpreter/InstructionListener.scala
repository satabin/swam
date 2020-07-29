package swam
package runtime
package internals
package interpreter

import swam.runtime.internals.instance.FunctionInstance

/**
  * @author Javier Cabrera-Arteaga on 2020-07-16
  */
trait InstructionListener[F[_]] {
  val wasiCheck : Boolean
  val covreport : Boolean
  val covshowmap: Boolean
  val filter : String
  val seed: Int
  def init(inner: AsmInst[F],index: Int, functionName: Option[String]): Unit
  def before(inner: AsmInst[F], index: Int, functionName: Option[String], frame: Frame[F]): Unit
  def after(inner: AsmInst[F], index: Int, frame: Frame[F], functionName: Option[String], result: Continuation[F]): Continuation[F]
}
