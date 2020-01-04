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

/** A reader is used to transform a web assembly value into a
  *  scala object.
  */
trait ValueReader[F[_], T] {

  def read(v: Value, m: Option[Memory[F]]): F[T]

  val swamType: ValType

}

/** A reader is used to transform a simple web assembly value into a
  *  scala object. A simple value van be read without memory instance.
  */
trait SimpleValueReader[F[_], T] extends ValueReader[F, T] {

  @inline
  final override def read(v: Value, m: Option[Memory[F]]): F[T] =
    read(v)

  def read(v: Value): F[T]

}
