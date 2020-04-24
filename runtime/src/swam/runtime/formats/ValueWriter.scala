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
package formats

import cats._
import cats.implicits._

/** A writer is used to transform a scala value into a
  *  web assembly value.
  */
trait ValueWriter[F[_], T] {

  def write(v: T, m: Option[Memory[F]]): F[Value]

  val swamType: ValType

}

object ValueWriter extends DefaultWriters

trait FunctionReturnWriter[F[_], T] {

  def write(v: T, m: Option[Memory[F]]): F[Vector[Value]]

  val swamTypes: Vector[ValType]

}

object FunctionReturnWriter {

  def apply[F[_], T](implicit ev: FunctionReturnWriter[F, T]): FunctionReturnWriter[F, T] = ev

  implicit def unitReturn[F[_]](implicit F: Applicative[F]): FunctionReturnWriter[F, Unit] =
    new FunctionReturnWriter[F, Unit] {
      def write(v: Unit, m: Option[Memory[F]]): F[Vector[Value]] =
        F.pure(Vector.empty)

      val swamTypes: Vector[ValType] =
        Vector.empty

    }

  implicit def singleReturn[F[_], T](implicit F: Applicative[F],
                                     writer: ValueWriter[F, T]): FunctionReturnWriter[F, T] =
    new FunctionReturnWriter[F, T] {
      def write(v: T, m: Option[Memory[F]]): F[Vector[Value]] =
        writer.write(v, m).map(Vector(_))

      val swamTypes: Vector[ValType] =
        Vector(writer.swamType)

    }

}

/** A writer is used to transform a scala value into a
  *  simple web assembly value. A simple value can be writter without memory instance.
  */
abstract class SimpleValueWriter[F[_], T](implicit F: MonadError[F, Throwable]) extends ValueWriter[F, T] {

  @inline
  final override def write(v: T, m: Option[Memory[F]]): F[Value] =
    F.pure(write(v))

  def write(v: T): Value

}
