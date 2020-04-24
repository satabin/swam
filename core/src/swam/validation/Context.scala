/*
 * Copyright 2018 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam
package validation

import cats._
import cats.implicits._

/** A validation context, used when performing module and instruction validation.
  */
case class Context[F[_]](types: Vector[FuncType],
                         funcs: Vector[FuncType],
                         tables: Vector[TableType],
                         mems: Vector[MemType],
                         globals: Vector[GlobalType],
                         locals: Vector[ValType],
                         labels: Vector[ResultType],
                         ret: Option[ResultType],
                         operands: List[OperandType],
                         unreachable: Boolean,
                         parent: Option[Context[F]])(implicit F: MonadError[F, Throwable]) {

  def withTypes(tps: Vector[FuncType]): Context[F] =
    copy(types = tps ++ types)

  def withFuncs(tps: Vector[FuncType]): Context[F] =
    copy(funcs = tps ++ funcs)

  def withTables(tps: Vector[TableType]): Context[F] =
    copy(tables = tps ++ tables)

  def withMems(tps: Vector[MemType]): Context[F] =
    copy(mems = tps ++ mems)

  def withGlobals(tps: Vector[GlobalType]): Context[F] =
    copy(globals = tps ++ globals)

  def withLocals(tps: Vector[ValType]): Context[F] =
    copy(locals = tps ++ locals)

  def withLabels(tps: Vector[ResultType]): Context[F] =
    copy(labels = tps ++ labels)

  def withReturn(tpe: Option[ResultType]): Context[F] =
    copy(ret = tpe)

  def push(optype: OperandType): Context[F] =
    copy(operands = optype :: operands)

  def push(optype: ValType): Context[F] =
    copy(operands = Val(optype) :: operands)

  def push(optypes: Vector[ValType]): Context[F] =
    optypes.foldLeft(this)(_.push(_))

  def pop(expected: ValType): F[Context[F]] =
    pop(Val(expected))

  def pop(expected: OperandType): F[Context[F]] =
    pop.flatMap {
      case (actual, ctx) =>
        if (actual == Unknown)
          F.pure(ctx)
        else if (expected == Unknown)
          F.pure(ctx)
        else if (expected == actual)
          F.pure(ctx)
        else
          F.raiseError(new ValidationException(s"expected $expected but got $actual"))
    }

  def pop(expected: Vector[ValType]): F[Context[F]] =
    F.tailRecM((expected.size - 1, this)) {
      case (idx, ctx) =>
        if (idx < 0)
          F.pure(Right(ctx))
        else
          ctx.pop(expected(idx)).flatMap {
            case ctx => F.pure(Left((idx - 1, ctx)))
          }
    }

  def pop: F[(OperandType, Context[F])] =
    operands match {
      case tpe :: rest => F.pure(tpe -> copy(operands = rest))
      case Nil =>
        if (unreachable)
          F.pure(Unknown -> this)
        else
          F.raiseError(new ValidationException("cannot pop from empty operand stack."))
    }

  def emptyOperands: F[Unit] =
    if (operands.isEmpty)
      F.pure(())
    else
      F.raiseError(new ValidationException("expected empty operand stack."))

  def withFreshOperands: Context[F] =
    copy(operands = Nil, unreachable = false)

  def markUnreachable: Context[F] =
    copy(operands = Nil, unreachable = true)

  private def extractType(bt: BlockType, loop: Boolean): F[(Vector[ValType], Int)] =
    bt match {
      case BlockType.NoType         => F.pure((Vector.empty, 0))
      case BlockType.ValueType(tpe) => F.pure((Vector(tpe), 0))
      case BlockType.FunctionType(tpeIdx) =>
        types.lift(tpeIdx) match {
          case Some(FuncType(params, results)) =>
            if (loop)
              F.pure((params, params.size))
            else
              F.pure((results, params.size))
          case None => F.raiseError(new ValidationException(s"unknown func type $tpeIdx"))
        }
    }

  def enterContext(bt: BlockType, loop: Boolean): F[Context[F]] =
    extractType(bt, loop).map {
      case (lbls, nbInputs) =>
        this
          .copy(labels = ResultType(lbls) +: labels,
                operands = operands.take(nbInputs),
                unreachable = false,
                parent = Some(this))
    }

  private def nbInputs(bt: BlockType): F[Int] =
    bt match {
      case BlockType.NoType         => F.pure(0)
      case BlockType.ValueType(tpe) => F.pure(0)
      case BlockType.FunctionType(tpeIdx) =>
        types.lift(tpeIdx) match {
          case Some(FuncType(params, results)) => F.pure(params.size)
          case None                            => F.raiseError(new ValidationException(s"unknown func type $tpeIdx"))
        }
    }

  def leaveContext(bt: BlockType, popInputs: Boolean): F[Context[F]] =
    nbInputs(bt).flatMap { nbInputs =>
      parent
        .map(ctx => if (popInputs) ctx.copy(operands = ctx.operands.drop(nbInputs)) else ctx)
        .liftTo[F](new ValidationException("no parent context"))
    }

}

sealed trait OperandType
case class Val(tpe: ValType) extends OperandType {
  override def toString = tpe.toString
}
case object Unknown extends OperandType
