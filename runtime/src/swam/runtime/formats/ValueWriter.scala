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

import shapeless._

/** A writer is used to transform a scala value into a
  *  web assembly value.
  */
trait ValueWriter[F[_], T] {

  def write(v: T, m: Option[Memory[F]]): F[Value]

  val swamType: ValType

}

object ValueWriter extends DefaultWriters

trait ValuesWriter[F[_], T] {

  def write(v: T, m: Option[Memory[F]]): F[Vector[Value]]

  val swamTypes: Vector[ValType]

}

object ValuesWriter {

  def apply[F[_], T](implicit ev: ValuesWriter[F, T]): ValuesWriter[F, T] = ev

  implicit def valuesWriterInstances[F[_]]: Contravariant[ValuesWriter[F, *]] = new Contravariant[ValuesWriter[F, *]] {
    def contramap[A, B](fa: ValuesWriter[F, A])(f: B => A): ValuesWriter[F, B] =
      new ValuesWriter[F, B] {
        def write(v: B, m: Option[Memory[F]]): F[Vector[Value]] = fa.write(f(v), m)
        val swamTypes: Vector[ValType] = fa.swamTypes
      }
  }

  implicit def unitReturn[F[_]](implicit F: Applicative[F]): ValuesWriter[F, Unit] =
    new ValuesWriter[F, Unit] {
      def write(v: Unit, m: Option[Memory[F]]): F[Vector[Value]] =
        F.pure(Vector.empty)

      val swamTypes: Vector[ValType] =
        Vector.empty

    }

  implicit def singleReturn[F[_], T](implicit F: Functor[F], writer: ValueWriter[F, T]): ValuesWriter[F, T] =
    new ValuesWriter[F, T] {
      def write(v: T, m: Option[Memory[F]]): F[Vector[Value]] =
        writer.write(v, m).map(Vector(_))

      val swamTypes: Vector[ValType] =
        Vector(writer.swamType)

    }

  implicit def hnilWriter[F[_]](implicit F: Applicative[F]): ValuesWriter[F, HNil] =
    new ValuesWriter[F, HNil] {
      def write(v: HNil, m: Option[Memory[F]]): F[Vector[Value]] = F.pure(Vector.empty)
      val swamTypes: Vector[ValType] = Vector.empty
    }

  implicit def hconsWriter[F[_], Head, Tail <: HList](implicit F: Monad[F],
                                                      head: ValueWriter[F, Head],
                                                      tail: ValuesWriter[F, Tail]): ValuesWriter[F, Head :: Tail] =
    new ValuesWriter[F, Head :: Tail] {
      def write(v: Head :: Tail, m: Option[Memory[F]]): F[Vector[Value]] =
        for {
          head <- head.write(v.head, m)
          tail <- tail.write(v.tail, m)
        } yield head +: tail
      val swamTypes: Vector[ValType] = head.swamType +: tail.swamTypes
    }

  implicit def productWriter[F[_], P <: Product, L <: HList](implicit F: Applicative[F],
                                                             gen: Generic.Aux[P, L],
                                                             writer: ValuesWriter[F, L]): ValuesWriter[F, P] =
    writer.contramap(gen.to(_))

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
