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
package runtime

import formats._

import internals.instance._
import internals.interpreter._

import cats._
import cats.implicits._

import java.nio.ByteBuffer

import scala.language.higherKinds

class Instance[F[_]](val module: Module[F],
                     _exports: Map[String, ExportedField],
                     interpreter: Interpreter[F],
                     globals: Vector[GlobalInstance],
                     memories: Vector[MemoryInstance],
                     funcs: Vector[CompiledFunction],
                     tables: Vector[TableInstance]) {
  self =>

  object exports {
    def asVar[T](name: String)(implicit F: MonadError[F, Throwable], format: ValueFormatter[T]): F[Var[F, T]] =
      _exports.get(name) match {
        case Some(ExportedField.Global(GlobalType(tpe, Mut.Var), index)) =>
          if (format.swamType == tpe)
            F.pure(new Var(globals(index)))
          else
            F.raiseError(new ConversionException(s"expected type $tpe but got type ${format.swamType}"))
        case Some(ExportedField.Global(_, _)) =>
          F.raiseError(new RuntimeException(s"cannot get a var for constant global named $name"))
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a var for type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown global named $name"))
      }

    def asVal[T](name: String)(implicit F: MonadError[F, Throwable], format: ValueFormatter[T]): F[Val[F, T]] =
      _exports.get(name) match {
        case Some(ExportedField.Global(GlobalType(tpe, _), index)) =>
          if (format.swamType == tpe)
            F.pure(new Var(globals(index)))
          else
            F.raiseError(new ConversionException(s"expected type $tpe but got type ${format.swamType}"))
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a var for type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown global named $name"))
      }

    def asFunction0[Ret](name: String)(implicit F: MonadError[F, Throwable],
                                       reader: ValueReader[Ret]): F[Function0[F[Ret]]] =
      _exports.get(name) match {
        case Some(ExportedField.Function(FuncType(Vector(), Vector(tpe)), idx)) =>
          if (tpe == reader.swamType)
            F.pure(() => interpreter.interpret(idx, Vector(), self).flatMap(res => wrap[Ret](res)))
          else
            F.raiseError(new RuntimeException(s"invalid return type (expected ${reader.swamType} but got $tpe"))
        case Some(ExportedField.Function(functype, idx)) =>
          F.raiseError(
            new RuntimeException(s"invalid function type (expected () => ${reader.swamType} but got $functype)"))
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a function for type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown function named $name"))
      }

    def asFunction0(name: String)(implicit F: MonadError[F, Throwable]): F[Function0[F[Unit]]] =
      _exports.get(name) match {
        case Some(ExportedField.Function(FuncType(Vector(), Vector()), idx)) =>
          F.pure(() => interpreter.interpret(idx, Vector(), self).flatMap(res => wrapUnit(res)))
        case Some(ExportedField.Function(functype, idx)) =>
          F.raiseError(new RuntimeException(s"invalid function type (expected () => Unit but got $functype)"))
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a function for type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown function named $name"))
      }

  }

  private def wrapUnit(res: Vector[Value])(implicit F: MonadError[F, Throwable]): F[Unit] =
    res match {
      case Vector() =>
        F.pure(())
      case _ =>
        throw new Exception("This is a bug")
    }

  private def wrap[Ret](res: Vector[Value])(implicit F: MonadError[F, Throwable], reader: ValueReader[Ret]): F[Ret] =
    res match {
      case Vector(ret) =>
        reader.read[F](ret)
      case ret =>
        throw new Exception("This is a bug")
    }

  private[swam] object global {
    def apply(idx: Int): Value =
      globals(idx).value

    def update(idx: Int, v: Value): Unit =
      globals(idx).value = v
  }

  private[swam] def memory(idx: Int): MemoryInstance =
    memories(idx)

  private[swam] def function(idx: Int): CompiledFunction =
    funcs(idx)

  private[swam] def table(idx: Int): TableInstance =
    tables(idx)

  private[swam] def tpe(idx: Int): FuncType =
    module.types(idx)

}

private sealed trait ExportedField {
  val tpe: Type
}

private object ExportedField {
  case class Global(tpe: GlobalType, idx: Int) extends ExportedField
  case class Function(tpe: FuncType, idx: Int) extends ExportedField
}

class Val[F[_], T](global: GlobalInstance)(implicit F: MonadError[F, Throwable], format: ValueReader[T]) {

  def apply(): F[T] =
    format.read[F](global.value)

  def value: F[T] =
    apply()

}

class Var[F[_], T](global: GlobalInstance)(implicit F: MonadError[F, Throwable], format: ValueFormatter[T])
    extends Val[F, T](global) {

  def update(v: T): F[Unit] =
    F.pure(global.value = format.write(v))

  def :=(v: T): F[Unit] =
    update(v)

}
