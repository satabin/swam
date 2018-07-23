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
package compiler

class Context private (parent: Option[Context]) {

  def this() = this(None)

  /** Opens a new child context. */
  def open(): Context =
    new Context(Some(this))

  /** Closes this context and returns its parent.
    *  The root context cannot be closed and is returned whenever
    *  it is tried to close it.
    */
  def close(): Context =
    parent match {
      case Some(ctx) => ctx
      case None      => this
    }

}
