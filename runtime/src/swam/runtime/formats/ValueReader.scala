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

/** A reader is used to transform a web assembly value into a
  *  scala object.
  */
trait ValueReader[F[_], T] {

  def read(v: Value, m: Option[Memory[F]]): F[T]

  val swamType: ValType

}

trait ValuesReader[F[_], T] {

  def read(vs: Vector[Value], mem: Option[Memory[F]]): F[T]

  def swamTypes: Vector[ValType]

}

object ValuesReader {

  def apply[F[_], T](implicit ev: ValuesReader[F, T]): ValuesReader[F, T] = ev

  implicit def valuesReaderInstances[F[_]: Functor]: Functor[ValuesReader[F, *]] =
    new Functor[ValuesReader[F, *]] {
      def map[A, B](fa: ValuesReader[F, A])(f: A => B): ValuesReader[F, B] =
        new ValuesReader[F, B] {
          def read(vs: Vector[Value], mem: Option[Memory[F]]): F[B] = fa.read(vs, mem).map(f(_))
          def swamTypes: Vector[ValType] = fa.swamTypes
        }
    }

  implicit def unitReader[F[_]](implicit F: ApplicativeError[F, Throwable]): ValuesReader[F, Unit] =
    new ValuesReader[F, Unit] {
      def read(vs: Vector[Value], mem: Option[Memory[F]]): F[Unit] =
        if (vs.isEmpty)
          F.unit
        else
          F.raiseError(new ConversionException(s"expected no return value but got ${vs.size}"))

      def swamTypes: Vector[ValType] =
        Vector.empty
    }

  implicit def singleReader[F[_], T](implicit F: ApplicativeError[F, Throwable],
                                     T: ValueReader[F, T]): ValuesReader[F, T] =
    new ValuesReader[F, T] {
      def read(vs: Vector[Value], mem: Option[Memory[F]]): F[T] =
        vs match {
          case Vector(v) =>
            T.read(v, mem)
          case _ =>
            F.raiseError(new ConversionException(s"expected a single value of type ${T.swamType} but got ${vs.size}"))
        }

      def swamTypes: Vector[ValType] =
        Vector(T.swamType)
    }

  implicit def hnilReader[F[_]](implicit F: ApplicativeError[F, Throwable]): ValuesReader[F, HNil] =
    new ValuesReader[F, HNil] {
      def read(vs: Vector[Value], mem: Option[Memory[F]]): F[HNil] =
        vs match {
          case Vector() => F.pure(HNil)
          case _        => F.raiseError(new ConversionException("expected empty values"))
        }

      def swamTypes: Vector[ValType] =
        Vector.empty
    }

  implicit def hconsReader[F[_], Head, Tail <: HList](implicit F: MonadError[F, Throwable],
                                                      Head: ValueReader[F, Head],
                                                      Tail: ValuesReader[F, Tail]): ValuesReader[F, Head :: Tail] =
    new ValuesReader[F, Head :: Tail] {
      def read(vs: Vector[Value], mem: Option[Memory[F]]): F[Head :: Tail] =
        if (vs.isEmpty)
          F.raiseError(new ConversionException(s"expected element of type ${Head.swamType} but got none"))
        else
          for {
            head <- Head.read(vs(0), mem)
            tail <- Tail.read(vs.tail, mem)
          } yield head :: tail

      def swamTypes: Vector[ValType] =
        Head.swamType +: Tail.swamTypes
    }

  implicit def productReader[F[_], P <: Product, L <: HList](implicit F: Functor[F],
                                                             gen: Generic.Aux[P, L],
                                                             reader: ValuesReader[F, L]): ValuesReader[F, P] =
    reader.map(gen.from(_))

}

object ValueReader extends DefaultReaders

/** A reader is used to transform a simple web assembly value into a
  *  scala object. A simple value van be read without memory instance.
  */
trait SimpleValueReader[F[_], T] extends ValueReader[F, T] {

  @inline
  final override def read(v: Value, m: Option[Memory[F]]): F[T] =
    read(v)

  def read(v: Value): F[T]

}
