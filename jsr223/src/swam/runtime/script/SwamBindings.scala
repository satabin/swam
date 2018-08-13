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
package script

import javax.script._

import java.util.{Set => JSet, Map => JMap, Collection => JCollection}

import scala.collection.JavaConverters._

import scala.language.higherKinds

/** Bindings that wrap a Swam [[Instance]].
  *
  * This `java.util.Map` is read-only and will throw an `UnsupportedOperationException`
  * for each call to a mutating method.
  */
class InstanceBindings[F[_]](private[script] val i: Instance[F]) extends AbstractBindings {

  def containsKey(key: Object): Boolean =
    key match {
      case key: String => i.exports.all.contains(key)
      case _           => false
    }

  def get(key: Object): Object =
    key match {
      case key: String => i.exports.all.getOrElse(key, null)
      case _           => null
    }

  def put(key: String, value: Object): Object =
    throw new UnsupportedOperationException("InstanceBindings.put")

  def remove(key: Object): Object =
    throw new UnsupportedOperationException("InstanceBindings.remove")

  def clear(): Unit =
    throw new UnsupportedOperationException("InstanceBindings.clear")

  def containsValue(value: Object): Boolean =
    i.exports.all.exists(_._2 == value)

  def entrySet(): JSet[JMap.Entry[String, Object]] =
    (i.exports.all: Map[String, Object]).asJava.entrySet()

  def isEmpty(): Boolean =
    i.exports.all.isEmpty

  def keySet(): JSet[String] =
    i.exports.all.keySet.asJava

  def size(): Int =
    i.exports.all.size

  def values(): JCollection[Object] =
    (i.exports.all.values: Iterable[Object]).asJavaCollection

}
