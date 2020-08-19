package swam.optin.coverage

import cats.effect.{Async, ContextShift, Sync}
import swam.optin.verifier.PathExtractor
import swam.runtime.internals.instance.FunctionInstance
import swam.runtime.internals.interpreter.{Asm, AsmInst}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuilder

/**
  * @author Javier Cabrera-Arteaga on 2020-08-18
  */
class CFGicator4Compiler[F[_]](val asm: Asm[F])(implicit F: Sync[F]) {

  def getBBIndexes(fun: FunctionInstance[F]): Array[Int] = {

    var leaders = Set[Int]()
    leaders += 0 // first instruction is a leader

    def getBBSaux(idx: Int): Unit = {

      if (idx < fun.code.length) {

        val inst: AsmInst[F] = fun.code(idx)

        inst match {
          // Every jump instruction target and the the next instruction is then a leader
          case x: asm.Jump => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: asm.JumpIf => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: asm.Br => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: asm.BrIf => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: asm.BrLabel => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: asm.BrTable => {
            x.lbls.foreach(l => {
              leaders += l.addr
            })

            leaders += x.dflt.addr
            leaders += idx + 1
          }
          case _ => {}
        }

        getBBSaux(idx + 1)
      }
    }

    getBBSaux(1)

    leaders.foreach(println)
    null
  }
}

object CFGicator4Compiler {
  def apply[F[_]: Async: ContextShift](asm: Asm[F]): CFGicator4Compiler[F] = new CFGicator4Compiler[F](asm)
}
