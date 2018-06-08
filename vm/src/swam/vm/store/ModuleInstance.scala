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

class ModuleInstance(
    types: Vector[FuncType],
    funcaddrs: Vector[Address],
    tableaddrs: Vector[Address],
    memaddrs: Vector[Address],
    globaladdrs: Vector[Address],
    exports: Vector[ExportInstance])(implicit store: Store) {

  object global {
    def apply(idx: Int): Value =
      store.globalValue(globaladdrs(idx))

    def update(idx: Int, v: Value): Unit =
      store.updateGlobalValue(globaladdrs(idx), v)
  }

  def memory(idx: Int): MemoryInstance =
    store.memory(memaddrs(idx))

  def function(idx: Int): FuncInstance =
    store.function(funcaddrs(idx))

  def table(idx: Int): TableInstance =
    store.table(tableaddrs(idx))

  def tpe(idx: Int): FuncType =
    types(idx)

}
