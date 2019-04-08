/*
 * Copyright 2019 Lucas Satabin
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
package annotations

/** `def` members marked with this annotation are exported as pure functions.
  * If you want to make side-effects, throw exctpion, and so on, please use [[effectful]] instead.
  *
  * The parameter and return types must have a [[swam.runtime.formats.SimpleValueFormatter SimpleValueFormatter]].
  *
  * Providing the `name` parameter will override the export name. By default the member name is taken.
  */
class pure(name: String) extends scala.annotation.StaticAnnotation {
  def this() = this("")
}

/** `def` members marked with this annotation are exported as a function
  *
  * The parameter and return types must have a [[swam.runtime.formats.SimpleValueFormatter SimpleValueFormatter]].
  *
  * Providing the `name` parameter will override the export name. By default the member name is taken.
  */
class effectful(name: String) extends scala.annotation.StaticAnnotation {
  def this() = this("")
}
