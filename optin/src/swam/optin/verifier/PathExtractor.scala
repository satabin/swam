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

  class State(val stack: Array[String], var fp: Int, val conditions: String) {

    def popEq(): String = {
      val r = stack(fp - 1)
      fp = fp - 1
      r
    }

    def pushEq(eq: String) = {
      stack(fp) = eq
      fp += 1
    }

    def drop(n: Int) = {
      fp -= n
    }

  }

  var count = 1

  def symbolicEval(fun: FunctionInstance[F]) = {
    // Create context 0
    val initial = new State(Array.ofDim[String](20), 0, "1")
    for (i <- fun.locals.indices) {
      initial.pushEq(s"x$i")
    }

    // start from code x
    var s = "digraph BST {"
    s += symbolicEvalAux(fun, 0, initial, s"$count", s"$count")
    s + "}"
  }

  def pushOperation(op: String, st: State, wrapLeft: Boolean = true, wrapRight: Boolean = true) = {
    val l1 =
      if (wrapLeft) s"(${st.popEq()})"
      else st.popEq()
    val l2 = if (wrapRight) s"(${st.popEq()})" else st.popEq()
    st.pushEq(s"$l2 $op $l1")
  }

  def symbolicEvalAux(fun: FunctionInstance[F],
                      pc: Int,
                      state: State,
                      prevContext: String,
                      context: String,
                      prev: Int = 0,
                      condition: String = ""): String = {

    //var realPi = pi
    val inst: AsmInst[F] = fun.code(pc)

    val result = if (state.fp > 0 && inst == asm.Return) s"\n${state.popEq()}" else ""
    //val rc = if (inst == asm.Return) state.hashCode() else pc

    var s =
      if (pc > 0)
        s"\t${prevContext}${prev} -> $context${pc} [label='${condition}'];\n$context${pc}[label='${inst.getClass.getSimpleName}${result}'];\n"
          .replace("'", "\"")
      else s"\n$context${pc}[label='${inst.getClass.getSimpleName}${result}'];\n".replace("'", "\"")

    inst match {
      case const: asm.I32Const => {
        state.pushEq(s"${const.v}")
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case const: asm.I64Const => {
        state.pushEq(s"${const.v}")
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case asm.I32Add => {
        pushOperation("+", state)
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case asm.I32Mul => {
        pushOperation("*", state)
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case asm.I32Sub => {
        pushOperation("-", state)
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case asm.I32DivS => {
        pushOperation("/", state)
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case asm.I32LtS => {
        pushOperation("<", state)
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case l: asm.LocalGet => {
        state.pushEq(s"l${l.idx}")
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case asm.I32Eqz => {
        state.pushEq("1")
        pushOperation("<", state)
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case x: asm.Jump => {
        s += symbolicEvalAux(fun, x.addr, state, context, context, pc, "")
      }
      case x: asm.JumpIf => {
        // fork in DFS mode
        // JUMP location
        val condition = state.popEq()

        //println(
        //  s"${x.addr} ${fun.code.slice(realPc - 1, toPc).zipWithIndex.map(t => s"${t._2 + realPc} ${t._1.getClass.getSimpleName}").mkString("\n")}")

        // Create a new node
        count += 1
        s += symbolicEvalAux(fun,
                             x.addr,
                             new State(state.stack.clone(), state.fp, s"${state.conditions} ^ ($condition)"),
                             context,
                             s"$count",
                             pc,
                             s"~($condition)")

        // true branch
        // Create a new node
        count += 1
        s += symbolicEvalAux(
          fun,
          pc + 1,
          new State(state.stack.clone(), state.fp, s"${state.conditions} ^ ~($condition)"),
          context,
          s"$count",
          pc,
          condition
        ) // false branch

        //realPi = s"${pi} /\\ ~($condition)" // else condition
      }
      case asm.Nop => {
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case x: asm.Call => {
        // TODO get arity and remove the parameters from the stack
        state.pushEq(s"f${x.fidx}()")
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case x: asm.Drop => {
        state.drop(x.n)
        s += symbolicEvalAux(fun, pc + 1, state, context, context, pc, "")
      }
      case asm.Unreachable => {
        //print(state.conditions)

        //println(s" => (TRAP)")
      }
      case asm.Return => {

        //print(state.conditions)

        //if (state.fp > 0)
        //println(s" => ${state.popEq()}")
      }
      case _ => {

        //symbolicEvalAux(fun, pc + 1, pi, prevPi, state)
        //println(s"$inst UNKNOWN behavior")
      }
    }

    //

    s
  }

}

object PathExtractor {
  def apply[F[_]: Async: ContextShift](asm: Asm[F]): PathExtractor[F] = new PathExtractor[F](asm)
}
