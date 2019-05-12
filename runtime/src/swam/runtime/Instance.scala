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

  /** Gives access to all exported fields of this instance. */
  object exports {

    /** Lists the exported fields and their type. */
    def list: Map[String, Type] =
      exps.mapValues(_.tpe)

    /** Returns a field by name. */
    def field(name: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
      exps.get(name) match {
        case Some(f) => F.pure(f)
        case None    => F.raiseError(new LinkException(s"unknown export named $name"))
      }

    /** Returns a global for given name. */
    def global(name: String)(implicit F: MonadError[F, Throwable]): F[Global[F]] =
      exps.get(name) match {
        case Some(g: Global[F]) =>
          F.pure(g)
        case Some(fld) =>
          F.raiseError(new LinkException(s"cannot get a global from type ${fld.tpe}"))
        case None =>
          F.raiseError(new LinkException(s"unknown global named $name"))
      }

    /** Returns a function for given name. */
    def function(name: String)(implicit F: MonadError[F, Throwable]): F[Function[F]] =
      exps.get(name) match {
        case Some(f: Function[F]) =>
          F.pure(f)
        case Some(fld) =>
          F.raiseError(new LinkException(s"cannot get a function from type ${fld.tpe}"))
        case None =>
          F.raiseError(new LinkException(s"unknown function named $name"))
      }

    /** Returns a memory for given name. */
    def memory(name: String)(implicit F: MonadError[F, Throwable]): F[Memory[F]] =
      exps.get(name) match {
        case Some(m: Memory[F]) => F.pure(m)
        case Some(fld) =>
          F.raiseError(new LinkException(s"cannot get a memory from type ${fld.tpe}"))
        case None =>
          F.raiseError(new LinkException(s"unknown global named $name"))
      }

    /** Returns a table for given name. */
    def table(name: String)(implicit F: MonadError[F, Throwable]): F[Table[F]] =
      exps.get(name) match {
        case Some(t: Table[F]) => F.pure(t)
        case Some(fld) =>
          F.raiseError(new LinkException(s"cannot get a table from type ${fld.tpe}"))
        case None =>
          F.raiseError(new LinkException(s"unknown global named $name"))
      }

    /** Access to wrapped typed versions of the exported fields. */
    object typed {

      /** Returns a global value for given name and type. */
      def global[T](name: String)(implicit F: MonadError[F, Throwable], reader: ValueReader[F, T]): F[T] =
        exps.get(name) match {
          case Some(g: Global[F]) =>
            if (reader.swamType == g.tpe.tpe)
              reader.read(g.get, memories.headOption)
            else
              F.raiseError(new ConversionException(s"expected type ${g.tpe.tpe} but got type ${reader.swamType}"))
          case Some(fld) =>
            F.raiseError(new LinkException(s"cannot get a var from type ${fld.tpe}"))
          case None =>
            F.raiseError(new LinkException(s"unknown global named $name"))
        }

      /** Returns a function for given name and type. */
      def function0[Ret](name: String)(implicit F: MonadError[F, Throwable],
                                       reader: ValueReader[F, Ret]): F[e.EFunction0[Ret, F]] =
        e.EFunction0[Ret, F](name, self)

      /** Returns a function for given name and type. */
      def procedure0(name: String)(implicit F: MonadError[F, Throwable]): F[e.EFunction0[Unit, F]] =
        e.EFunction0[F](name, self)

      /** Returns a function for given name and type. */
      def function1[P1, Ret](name: String)(implicit F: MonadError[F, Throwable],
                                           writer1: ValueWriter[F, P1],
                                           reader: ValueReader[F, Ret]): F[e.EFunction1[P1, Ret, F]] =
        e.EFunction1(name, self)

      /** Returns a function for given name and type. */
      def procedure1[P1](name: String)(implicit F: MonadError[F, Throwable],
                                       writer1: ValueWriter[F, P1]): F[e.EFunction1[P1, Unit, F]] =
        e.EFunction1[P1, F](name, self)

      /** Returns a function for given name and type. */
      def function2[P1, P2, Ret](name: String)(implicit F: MonadError[F, Throwable],
                                               writer1: ValueWriter[F, P1],
                                               writer2: ValueWriter[F, P2],
                                               reader: ValueReader[F, Ret]): F[e.EFunction2[P1, P2, Ret, F]] =
        e.EFunction2(name, self)

      /** Returns a function for given name and type. */
      def procedure2[P1, P2](name: String)(implicit F: MonadError[F, Throwable],
                                           writer1: ValueWriter[F, P1],
                                           writer2: ValueWriter[F, P2]): F[e.EFunction2[P1, P2, Unit, F]] =
        e.EFunction2[P1, P2, F](name, self)

      /** Returns a function for given name and type. */
      def function3[P1, P2, P3, Ret](name: String)(implicit F: MonadError[F, Throwable],
                                                   writer1: ValueWriter[F, P1],
                                                   writer2: ValueWriter[F, P2],
                                                   writer3: ValueWriter[F, P3],
                                                   reader: ValueReader[F, Ret]): F[e.EFunction3[P1, P2, P3, Ret, F]] =
        e.EFunction3(name, self)

      /** Returns a function for given name and type. */
      def procedure3[P1, P2, P3](name: String)(implicit F: MonadError[F, Throwable],
                                               writer1: ValueWriter[F, P1],
                                               writer2: ValueWriter[F, P2],
                                               writer3: ValueWriter[F, P3]): F[e.EFunction3[P1, P2, P3, Unit, F]] =
        e.EFunction3[P1, P2, P3, F](name, self)

      /** Returns a function for given name and type. */
      def function4[P1, P2, P3, P4, Ret](name: String)(
          implicit F: MonadError[F, Throwable],
          writer1: ValueWriter[F, P1],
          writer2: ValueWriter[F, P2],
          writer3: ValueWriter[F, P3],
          writer4: ValueWriter[F, P4],
          reader: ValueReader[F, Ret]): F[e.EFunction4[P1, P2, P3, P4, Ret, F]] =
        e.EFunction4(name, self)

      /** Returns a function for given name and type. */
      def procedure4[P1, P2, P3, P4](name: String)(
          implicit F: MonadError[F, Throwable],
          writer1: ValueWriter[F, P1],
          writer2: ValueWriter[F, P2],
          writer3: ValueWriter[F, P3],
          writer4: ValueWriter[F, P4]): F[e.EFunction4[P1, P2, P3, P4, Unit, F]] =
        e.EFunction4[P1, P2, P3, P4, F](name, self)

    }

  }

  private[runtime] var exps: Map[String, Interface[F, Type]] = Map.empty
  private[runtime] var globals: Vector[Global[F]] = Vector.empty
  private[runtime] var memories: Vector[Memory[F]] = Vector.empty
  private[runtime] var funcs: Vector[Function[F]] = Vector.empty
  private[runtime] var tables: Vector[Table[F]] = Vector.empty

  private type Funs = Seq[(Int, Function[F])]

  private def initTables(implicit F: MonadError[F, Throwable]): F[Funs] =
    F.tailRecM[(Int, Funs), Funs]((0, Seq.empty[(Int, Function[F])])) {
      case (idx, acc) =>
        if (idx >= module.elems.size)
          F.pure(Right(acc))
        else
          module.elems(idx) match {
            case CompiledElem(coffset, init) =>
              interpreter.interpretInit(ValType.I32, coffset, self).flatMap { roffset =>
                val offset = (roffset.get & 0X00000000FFFFFFFFL).toInt
                if (offset < 0 || init.size + offset > tables(0).size) {
                  F.raiseError(new LinkException("Overflow in table initialization"))
                } else {
                  val funs =
                    for (initi <- 0 until init.size)
                      yield (offset + initi, funcs(init(initi)))
                  F.pure(Left((idx + 1, acc ++ funs)))
                }
              }
          }
    }

  private type Mems = List[(Int, ByteBuffer)]

  private def initMems(implicit F: MonadError[F, Throwable]): F[Mems] =
    F.tailRecM[(Int, Mems), Mems]((0, Nil)) {
      case (idx, acc) =>
        if (idx >= module.data.size)
          F.pure(Right(acc))
        else
          module.data(idx) match {
            case CompiledData(coffset, init) =>
              interpreter.interpretInit(ValType.I32, coffset, self).flatMap { roffset =>
                val offset = (roffset.get & 0X00000000FFFFFFFFL).toInt
                if (offset < 0 || init.capacity + offset > memories(0).size)
                  F.raiseError(new LinkException("Overflow in memory initialization"))
                else
                  F.pure(Left((idx + 1, acc :+ (offset, init))))
              }
          }
    }

  private def setvalues(elems: Funs, data: Mems)(implicit F: MonadError[F, Throwable]): F[Unit] =
    F.pure {
      elems.foreach {
        case (idx, f) => tables(0)(idx) = f
      }
    } >>
      data.foldM(()) {
        case (_, (idx, m)) => memories(0).writeBytes(idx, m)
      }

  private def start(implicit F: MonadError[F, Throwable]): F[Unit] =
    module.start match {
      case Some(start) => interpreter.interpret(start, Vector(), self).map(_ => ())
      case None        => F.pure(())
    }

  private[runtime] def init(implicit F: MonadError[F, Throwable]): F[Unit] =
    for {
      // initialize tables
      elems <- initTables
      // now initialize memories
      data <- initMems
      // perform atomic initialization
      _ <- setvalues(elems, data)
      // and finally start the instance
      _ <- start
    } yield ()

}
