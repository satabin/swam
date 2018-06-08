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
package vm
package store

/** A memory backed by arrays.
 */
class SimpleStore(
    funcs: Array[FuncInstance],
    tables: Array[TableInstance],
    mems: Array[MemoryInstance],
    globals: Array[GlobalInstance]) extends Store {

  def globalValue(addr: Address): Value =
    globals(addr).value

  def updateGlobalValue(addr: Address, v: Value): Unit = {
    val g = globals(addr)
    if (g.mutable)
      g.value = v
    else
      throw new SwamException("Setting immutable global value")
  }

  def memory(addr: Address): MemoryInstance =
    mems(addr)

  def function(addr: Address): FuncInstance =
    funcs(addr)

  def table(addr: Address): TableInstance =
    tables(addr)

}
