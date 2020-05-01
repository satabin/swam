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

/** A formatter is both a [[ValueReader]] and a [[ValueWriter]].
  */
trait ValueFormatter[F[_], T] extends ValueReader[F, T] with ValueWriter[F, T]

object ValueFormatter {

  implicit def formatterWithBoth[F[_], T](implicit reader: ValueReader[F, T],
                                          writer: ValueWriter[F, T]): ValueFormatter[F, T] =
    new ValueFormatter[F, T] {
      def read(v: Value, m: Option[Memory[F]]): F[T] = reader.read(v, m)
      def write(v: T, m: Option[Memory[F]]): F[Value] = writer.write(v, m)
      val swamType: ValType = reader.swamType

    }

}

abstract class SimpleValueFormatter[F[_], T](implicit F: MonadError[F, Throwable])
    extends SimpleValueWriter[F, T]
    with SimpleValueReader[F, T]
    with ValueFormatter[F, T]

object SimpleValueFormatter {

  implicit def formatterWithBoth[F[_], T](implicit F: MonadError[F, Throwable],
                                          reader: SimpleValueReader[F, T],
                                          writer: SimpleValueWriter[F, T]): SimpleValueFormatter[F, T] =
    new SimpleValueFormatter[F, T] {
      def read(v: Value): F[T] = reader.read(v)
      def write(v: T): Value = writer.write(v)
      val swamType: ValType = reader.swamType

    }

}
