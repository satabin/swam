package swam.optin.verifier

import swam.ValType
import swam.runtime.Function
import swam.runtime.internals.instance.FunctionInstance
import cats.implicits._
import cats.effect._
import swam.runtime.internals.interpreter.{Asm, AsmInst}

/**
  * @author Javier Cabrera-Arteaga on 2020-06-23
  */
class PathExtractor[F[_]](val asm: Asm[F])(implicit F: Sync[F]) {

  case class Symbol(name: String, tpe: ValType)
  private var fp = 0

  class State(val stack: Array[String], var fp: Int) {

    def popEq(): String = {
      val r = stack(fp - 1)
      fp = fp - 1
      r
    }

    def pushEq(eq: String) = {
      stack(fp) = eq
      fp += 1
    }

  }

  def symbolicEval(fun: FunctionInstance[F]) = {
    // Create context 0
    val initial = new State(Array.ofDim[String](20), 0)
    for (i <- fun.locals.indices) {
      initial.pushEq(s"x$i")
    }

    // start from code x
    symbolicEvalAux(fun, 0, fun.code.length, pi = "true", initial)

  }

  def pushOperation(op: String, st: State, wrapLeft: Boolean = true, wrapRight: Boolean = true) = {
    val l1 = if (wrapLeft) s"(${st.popEq()})" else st.popEq()
    val l2 = if (wrapRight) s"(${st.popEq()})" else st.popEq()
    st.pushEq(s"$l1 $op $l2")
  }

  def symbolicEvalAux(fun: FunctionInstance[F], pc: Int, toPc: Int, pi: String, state: State): Int = {

    // println(state.stack.mkString(","))

    println(s"; $pi [$pc-$toPc]")
    var realPc = pc
    while (realPc < toPc) {
      val inst: AsmInst[F] = fun.code(realPc)
      inst match {
        case const: asm.I32Const => {
          state.pushEq(s"${const.v}")
          realPc += 1
        }
        case asm.I32Add => {
          pushOperation("+", state)
          realPc += 1
        }
        case asm.I32Mul => {
          pushOperation("*", state)
          realPc += 1
        }
        case asm.I32Sub => {
          pushOperation("-", state)
          realPc += 1
        }
        case asm.I32DivS => {
          pushOperation("/", state)
          realPc += 1
        }
        case asm.I32LtS => {
          pushOperation(">", state)
          realPc += 1
        }
        case l: asm.LocalGet => {
          state.pushEq(s"l${l.idx}")
          realPc += 1
        }
        case asm.I32Eqz => {
          state.pushEq("1")
          pushOperation(">", state)
          realPc += 1
        }
        case x: asm.JumpIf => {
          // fork
          val condition = state.popEq()
          realPc = symbolicEvalAux(fun,
                                   realPc + 1,
                                   x.addr - 1,
                                   s"$pi /\\ ($condition)",
                                   new State(state.stack.clone(), state.fp))

          realPc = symbolicEvalAux(fun, realPc + 1, toPc, s"$pi /\\ ~($condition)", state)
        }
        case _ => {
          realPc += 1
          println(s"$inst UNKNOWN behavior")
        }
      }
    }
    realPc
  }

}

object PathExtractor {
  def apply[F[_]: Async: ContextShift](asm: Asm[F]): PathExtractor[F] = new PathExtractor[F](asm)
}
