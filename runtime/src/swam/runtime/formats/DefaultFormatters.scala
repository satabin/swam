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

/** A collection of default formatters for primitive Scala types.
  */
trait DefaultFormatters {

  private def raise[T, F[_]](msg: String)(implicit F: MonadError[F, Throwable]): F[T] =
    F.raiseError(new ConversionException(msg))

  implicit def intFormatter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueFormatter[F, Int] =
    new SimpleValueFormatter[F, Int] {
      def read(v: Value): F[Int] =
        v match {
          case Value.Int32(i) => F.pure(i)
          case _              => raise(s"expected i32 but got ${v.tpe}")
        }
      def write(i: Int): Value =
        Value.Int32(i)
      val swamType: ValType =
        ValType.I32
    }

  implicit def longFormatter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueFormatter[F, Long] =
    new SimpleValueFormatter[F, Long] {
      def read(v: Value): F[Long] =
        v match {
          case Value.Int64(l) => F.pure(l)
          case _              => raise(s"expected i64 but got ${v.tpe}")
        }
      def write(l: Long): Value =
        Value.Int64(l)
      val swamType: ValType =
        ValType.I64
    }

  implicit def floatFormatter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueFormatter[F, Float] =
    new SimpleValueFormatter[F, Float] {
      def read(v: Value): F[Float] =
        v match {
          case Value.Float32(f) => F.pure(f)
          case _                => raise(s"expected f32 but got ${v.tpe}")
        }
      def write(f: Float): Value =
        Value.Float32(f)
      val swamType: ValType =
        ValType.F32
    }

  implicit def doubleFormatter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueFormatter[F, Double] =
    new SimpleValueFormatter[F, Double] {
      def read(v: Value): F[Double] =
        v match {
          case Value.Float64(d) => F.pure(d)
          case _                => raise(s"expected f64 but got ${v.tpe}")
        }
      def write(d: Double): Value =
        Value.Float64(d)
      val swamType: ValType =
        ValType.F64
    }

  implicit def boolFormatter[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueFormatter[F, Boolean] =
    new SimpleValueFormatter[F, Boolean] {
      def read(v: Value): F[Boolean] =
        v match {
          case Value.Int32(i) => F.pure(i != 0)
          case _              => raise(s"expected i32 but got ${v.tpe}")
        }
      def write(b: Boolean): Value =
        Value.Int32(if (b) 1 else 0)
      val swamType: ValType =
        ValType.I32
    }

}

object DefaultFormatters extends DefaultFormatters
