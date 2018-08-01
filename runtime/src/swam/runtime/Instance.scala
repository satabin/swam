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
import runtime.{imports => i}
import runtime.{exports => e}

import internals.instance._
import internals.interpreter._

import cats._
import cats.implicits._

import java.nio.ByteBuffer

import scala.language.higherKinds

class Instance[F[_]](val module: Module[F], private[runtime] val interpreter: Interpreter[F]) {
  self =>

  private[runtime] var exps: Map[String, Interface[F, Type]] = Map.empty
  private[runtime] var globals: Vector[Global[F]] = Vector.empty
  private[runtime] var memories: Vector[Memory[F]] = Vector.empty
  private[runtime] var funcs: Vector[Function[F]] = Vector.empty
  private[runtime] var tables: Vector[Table[F]] = Vector.empty

  object exports {

    def field(name: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
      exps.get(name) match {
        case Some(f) => F.pure(f)
        case None =>          F.raiseError(new RuntimeException(s"unknown export named $name"))
      }

    def asVar[T](name: String)(implicit F: MonadError[F, Throwable], format: ValueFormatter[T]): F[e.Var[F, T]] =
      exps.get(name) match {
        case Some(g: Global[F]) if g.tpe.mut == Mut.Var =>
          if (format.swamType == g.tpe)
            F.pure(new e.Var(g))
          else
            F.raiseError(new ConversionException(s"expected type ${g.tpe} but got type ${format.swamType}"))
        case Some(_: Global[F]) =>
          F.raiseError(new RuntimeException(s"cannot get a var for constant global named $name"))
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a var for type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown global named $name"))
      }

    def asVal[T](name: String)(implicit F: MonadError[F, Throwable], format: ValueFormatter[T]): F[e.Val[F, T]] =
      exps.get(name) match {
        case Some(g: Global[F]) =>
          if (format.swamType == g.tpe)
            F.pure(new e.Val(g))
          else
            F.raiseError(new ConversionException(s"expected type ${g.tpe} but got type ${format.swamType}"))
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a var for type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown global named $name"))
      }

    def asFunction0[Ret](name: String)(implicit F: MonadError[F, Throwable],
                                       reader: ValueReader[Ret]): F[e.EFunction0[Ret, F]] =
      e.EFunction0[Ret, F](name, self)

    def asFunction0(name: String)(implicit F: MonadError[F, Throwable]): F[e.EFunction0[Unit, F]] =
      e.EFunction0[F](name, self)

    def asFunction1[P1, Ret](name: String)(implicit F: MonadError[F, Throwable],
                                           writer1: ValueWriter[P1],
                                           reader: ValueReader[Ret]): F[e.EFunction1[P1, Ret, F]] =
      e.EFunction1(name, self)

  }

  private[swam] object global {
    def apply(idx: Int): Value =
      globals(idx).get

    def update(idx: Int, v: Value): Unit =
      globals(idx).set(v)
  }

  private[swam] def memory(idx: Int): Memory[F] =
    memories(idx)

  private[swam] def function(idx: Int): Function[F] =
    funcs(idx)

  private[swam] def table(idx: Int): Table[F] =
    tables(idx)

  private[swam] def tpe(idx: Int): FuncType =
    module.types(idx)

}
