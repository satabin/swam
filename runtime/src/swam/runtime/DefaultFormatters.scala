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

import cats._

import scala.language.higherKinds

/** A collection of default formatters for primitive Scala types.
  */
trait DefaultFormatters {

  private def raise[T, F[_]](msg: String)(implicit F: MonadError[F, Throwable]): F[T] =
    F.raiseError(new ConversionException(msg))

  implicit val intFormatter: ValueFormatter[Int] = new ValueFormatter[Int] {
    def read[F[_]](v: Value)(implicit F: MonadError[F, Throwable]): F[Int] =
      v match {
        case Value.Int32(i) => F.pure(i)
        case _              => raise(s"expected i32 but got ${Value.tpe(v)}")
      }
    def write(i: Int): Value =
      Value.Int32(i)
  }

  implicit val longFormatter: ValueFormatter[Long] = new ValueFormatter[Long] {
    def read[F[_]](v: Value)(implicit F: MonadError[F, Throwable]): F[Long] =
      v match {
        case Value.Int32(i) => F.pure(i)
        case Value.Int64(l) => F.pure(l)
        case _              => raise(s"expected i32 or i64 but got ${Value.tpe(v)}")
      }
    def write(l: Long): Value =
      Value.Int64(l)
  }

  implicit val floatFormatter: ValueFormatter[Float] = new ValueFormatter[Float] {
    def read[F[_]](v: Value)(implicit F: MonadError[F, Throwable]): F[Float] =
      v match {
        case Value.Float32(f) => F.pure(f)
        case _                => raise(s"expected f32 but got ${Value.tpe(v)}")
      }
    def write(f: Float): Value =
      Value.Float32(f)
  }

  implicit val doubleFormatter: ValueFormatter[Double] = new ValueFormatter[Double] {
    def read[F[_]](v: Value)(implicit F: MonadError[F, Throwable]): F[Double] =
      v match {
        case Value.Float32(f) => F.pure(f)
        case Value.Float64(d) => F.pure(d)
        case _                => raise(s"expected f32 or f64 but got ${Value.tpe(v)}")
      }
    def write(d: Double): Value =
      Value.Float64(d)
  }

  implicit val boolFormatter: ValueFormatter[Boolean] = new ValueFormatter[Boolean] {
    def read[F[_]](v: Value)(implicit F: MonadError[F, Throwable]): F[Boolean] =
      v match {
        case Value.Int32(i) => F.pure(i != 0)
        case _              => raise(s"expected i32 but got ${Value.tpe(v)}")
      }
    def write(b: Boolean): Value =
      Value.Int32(if (b) 1 else 0)
  }

}

object DefaultFormatters extends DefaultFormatters
