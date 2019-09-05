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
package imports

import scala.language.implicitConversions

trait Elem[TC[_]] {
  type T
  val value: T
  val typeclass: TC[T]
}

object Elem {

  implicit def fromValue[V, TC[_]](v: V)(implicit V: TC[V]): Elem[TC] =
    new Elem[TC] {
      type T = V
      val value = v
      val typeclass = V
    }

}

/** A map whose elements may be of any type as long as a given typeclass
  *  for them is provided.
  */
class TCMap[K, TC[_]] private (private val wrapped: Map[K, Elem[TC]]) {

  /** Returns the element identified by the given key `k`.
    *  Returns `None` if no such element exists.
    */
  def get(k: K): Option[Elem[TC]] =
    wrapped.get(k)

  /** Returns an updated map with the given value added to it. */
  def updated[V](k: K, v: V)(implicit V: TC[V]): TCMap[K, TC] =
    new TCMap(wrapped.updated(k, new Elem[TC] {
      type T = V
      val value = v
      val typeclass = V
    }))

  /** Alias for `updated`. */
  def &[V](kv: (K, V))(implicit V: TC[V]): TCMap[K, TC] =
    updated(kv._1, kv._2)

}

object TCMap {

  def empty[K, TC[_]]: TCMap[K, TC] =
    new TCMap(Map.empty[K, Elem[TC]])

  def apply[K, TC[_]](pairs: (K, Elem[TC])*): TCMap[K, TC] =
    new TCMap(Map(pairs: _*))

}
