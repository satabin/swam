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

/** A collection of default readers for primitive Scala types.
  */
trait DefaultReaders {

  private def raise[T, F[_]](msg: String)(implicit F: MonadError[F, Throwable]): F[T] =
    F.raiseError(new ConversionException(msg))

  implicit def byteReader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Byte] =
    new SimpleValueReader[F, Byte] {
      def read(v: Value): F[Byte] =
        v match {
          case Value.Int32(i) => F.pure(i.toByte)
          case _              => raise(s"expected i32 but got ${v.tpe}")
        }
      val swamType: ValType =
        ValType.I32
    }

  implicit def shortReader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Short] =
    new SimpleValueReader[F, Short] {
      def read(v: Value): F[Short] =
        v match {
          case Value.Int32(i) => F.pure(i.toShort)
          case _              => raise(s"expected i32 but got ${v.tpe}")
        }
      val swamType: ValType =
        ValType.I32
    }

  implicit def intReader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Int] =
    new SimpleValueReader[F, Int] {
      def read(v: Value): F[Int] =
        v match {
          case Value.Int32(i) => F.pure(i)
          case _              => raise(s"expected i32 but got ${v.tpe}")
        }
      val swamType: ValType =
        ValType.I32
    }

  implicit def longReader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Long] =
    new SimpleValueReader[F, Long] {
      def read(v: Value): F[Long] =
        v match {
          case Value.Int64(l) => F.pure(l)
          case _              => raise(s"expected i64 but got ${v.tpe}")
        }
      val swamType: ValType =
        ValType.I64
    }

  implicit def floatReader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Float] =
    new SimpleValueReader[F, Float] {
      def read(v: Value): F[Float] =
        v match {
          case Value.Float32(f) => F.pure(f)
          case _                => raise(s"expected f32 but got ${v.tpe}")
        }
      val swamType: ValType =
        ValType.F32
    }

  implicit def doubleReader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Double] =
    new SimpleValueReader[F, Double] {
      def read(v: Value): F[Double] =
        v match {
          case Value.Float64(d) => F.pure(d)
          case _                => raise(s"expected f64 but got ${v.tpe}")
        }
      val swamType: ValType =
        ValType.F64
    }

  implicit def boolReader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Boolean] =
    new SimpleValueReader[F, Boolean] {
      def read(v: Value): F[Boolean] =
        v match {
          case Value.Int32(i) => F.pure(i != 0)
          case _              => raise(s"expected i32 but got ${v.tpe}")
        }
      val swamType: ValType =
        ValType.I32
    }

}

/** A collection of default writers for primitive Scala types.
  */
trait DefaultWriters {

  private def raise[T, F[_]](msg: String)(implicit F: MonadError[F, Throwable]): F[T] =
    F.raiseError(new ConversionException(msg))

  implicit def byteWriter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Byte] =
    new SimpleValueWriter[F, Byte] {
      def write(i: Byte): Value =
        Value.Int32(i)
      val swamType: ValType =
        ValType.I32
    }

  implicit def shortWriter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Short] =
    new SimpleValueWriter[F, Short] {
      def write(i: Short): Value =
        Value.Int32(i)
      val swamType: ValType =
        ValType.I32
    }

  implicit def intWriter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Int] =
    new SimpleValueWriter[F, Int] {
      def write(i: Int): Value =
        Value.Int32(i)
      val swamType: ValType =
        ValType.I32
    }

  implicit def longWriter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Long] =
    new SimpleValueWriter[F, Long] {
      def write(l: Long): Value =
        Value.Int64(l)
      val swamType: ValType =
        ValType.I64
    }

  implicit def floatWriter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Float] =
    new SimpleValueWriter[F, Float] {
      def write(f: Float): Value =
        Value.Float32(f)
      val swamType: ValType =
        ValType.F32
    }

  implicit def doubleWriter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Double] =
    new SimpleValueWriter[F, Double] {
      def write(d: Double): Value =
        Value.Float64(d)
      val swamType: ValType =
        ValType.F64
    }

  implicit def boolWriter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Boolean] =
    new SimpleValueWriter[F, Boolean] {
      def write(b: Boolean): Value =
        Value.Int32(if (b) 1 else 0)
      val swamType: ValType =
        ValType.I32
    }

}
