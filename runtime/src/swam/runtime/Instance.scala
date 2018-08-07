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

/** A module instance that has already been initialized.
  *
  * @param module The [[Module]] of this instance.
  */
class Instance[F[_]] private[runtime] (val module: Module[F], private[runtime] val interpreter: Interpreter[F]) {
  self =>

  private[runtime] var exps: Map[String, Interface[F, Type]] = Map.empty
  private[runtime] var globals: Vector[Global[F]] = Vector.empty
  private[runtime] var memories: Vector[Memory[F]] = Vector.empty
  private[runtime] var funcs: Vector[Function[F]] = Vector.empty
  private[runtime] var tables: Vector[Table[F]] = Vector.empty

  /** Gives access to all exported fields of this instance. */
  object exports {

    /** Lists the exported fields and their type. */
    def list: Map[String, Type] =
      exps.mapValues(_.tpe)

    /** Returns a field by name. */
    def field(name: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
      exps.get(name) match {
        case Some(f) => F.pure(f)
        case None    => F.raiseError(new RuntimeException(s"unknown export named $name"))
      }

    /** Returns a global for given name. */
    def global(name: String)(implicit F: MonadError[F, Throwable]): F[Global[F]] =
      exps.get(name) match {
        case Some(g: Global[F]) =>
          F.pure(g)
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a global from type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown global named $name"))
      }

    /** Returns a function for given name. */
    def function(name: String)(implicit F: MonadError[F, Throwable]): F[Function[F]] =
      exps.get(name) match {
        case Some(f: Function[F]) =>
          F.pure(f)
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a function from type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown function named $name"))
      }

    /** Returns a memory for given name. */
    def memory(name: String)(implicit F: MonadError[F, Throwable]): F[Memory[F]] =
      exps.get(name) match {
        case Some(m: Memory[F]) => F.pure(m)
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a memory from type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown global named $name"))
      }

    /** Returns a table for given name. */
    def table(name: String)(implicit F: MonadError[F, Throwable]): F[Table[F]] =
      exps.get(name) match {
        case Some(t: Table[F]) => F.pure(t)
        case Some(fld) =>
          F.raiseError(new RuntimeException(s"cannot get a table from type ${fld.tpe}"))
        case None =>
          F.raiseError(new RuntimeException(s"unknown global named $name"))
      }

    /** Access to wrapped typed versions of the exported fields. */
    object typed {

      /** Returns a global value for given name and type. */
      def global[T](name: String)(implicit F: MonadError[F, Throwable], reader: ValueReader[T]): F[T] =
        exps.get(name) match {
          case Some(g: Global[F]) =>
            if (reader.swamType == g.tpe.tpe)
              reader.read[F](g.get)
            else
              F.raiseError(new ConversionException(s"expected type ${g.tpe.tpe} but got type ${reader.swamType}"))
          case Some(fld) =>
            F.raiseError(new RuntimeException(s"cannot get a var from type ${fld.tpe}"))
          case None =>
            F.raiseError(new RuntimeException(s"unknown global named $name"))
        }

      /** Returns a function for given name and type. */
      def function0[Ret](name: String)(implicit F: MonadError[F, Throwable],
                                       reader: ValueReader[Ret]): F[e.EFunction0[Ret, F]] =
        e.EFunction0[Ret, F](name, self)

      /** Returns a function for given name and type. */
      def procedure0(name: String)(implicit F: MonadError[F, Throwable]): F[e.EFunction0[Unit, F]] =
        e.EFunction0[F](name, self)

      /** Returns a function for given name and type. */
      def function1[P1, Ret](name: String)(implicit F: MonadError[F, Throwable],
                                           writer1: ValueWriter[P1],
                                           reader: ValueReader[Ret]): F[e.EFunction1[P1, Ret, F]] =
        e.EFunction1(name, self)

      /** Returns a function for given name and type. */
      def procedure1[P1](name: String)(implicit F: MonadError[F, Throwable],
                                       writer1: ValueWriter[P1]): F[e.EFunction1[P1, Unit, F]] =
        e.EFunction1[P1, F](name, self)

      /** Returns a function for given name and type. */
      def function2[P1, P2, Ret](name: String)(implicit F: MonadError[F, Throwable],
                                               writer1: ValueWriter[P1],
                                               writer2: ValueWriter[P2],
                                               reader: ValueReader[Ret]): F[e.EFunction2[P1, P2, Ret, F]] =
        e.EFunction2(name, self)

      /** Returns a function for given name and type. */
      def procedure2[P1, P2](name: String)(implicit F: MonadError[F, Throwable],
                                           writer1: ValueWriter[P1],
                                           writer2: ValueWriter[P2]): F[e.EFunction2[P1, P2, Unit, F]] =
        e.EFunction2[P1, P2, F](name, self)

    }

  }

  private[runtime] object global {
    def apply(idx: Int): Value =
      globals(idx).get

    def update(idx: Int, v: Value)(implicit F: MonadError[F, Throwable]): F[Unit] =
      globals(idx) match {
        case g: GlobalInstance[F] => g.unsafeset(v)
        case g                    => g.set(v)
      }
  }

  private[runtime] def memory(idx: Int): Memory[F] =
    memories(idx)

  private[runtime] def function(idx: Int): Function[F] =
    funcs(idx)

  private[runtime] def table(idx: Int): Table[F] =
    tables(idx)

  private[runtime] def tpe(idx: Int): FuncType =
    module.types(idx)

}
