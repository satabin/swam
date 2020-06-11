package swam
package runtime
package internals
package interpreter

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
trait InstructionListener[F[_]] {

  def init(inner: AsmInst[F]): Unit
  def before(inner: AsmInst[F], frame: Frame[F]): Unit
  def after(inner: AsmInst[F], frame: Frame[F], result: Continuation[F]): Continuation[F]
}
