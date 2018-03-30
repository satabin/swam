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

package swam.runtime
package instance

import store.Store

/** The runtime representation of a module instance.
 *  A module operates over a memory.
 *  The runtime representation of a module instance in memory is as follows.
 *  {{{
 *  type_start: i64
 *  func_start: i64
 *  table_start: i64
 *  mem_start: i64
 *  global_start: i64
 *  export_start: i64
 *  t_0: i64
 *  ...
 *  t_n: i64
 *  f_0: i64
 *  ...
 *  f_m: i64
 *  t_0: i64
 *  ...
 *  t_o; i64
 *  m_0: i64
 *  ...
 *  m_p: i64
 *  g_0: i64
 *  ..
 *  g_q: i64
 *  e_0: <export>
 *  ...
 *  e_r: <export>
 *  }}}
 *
 *  An export is of the form:
 *  {{{
 *  n_size: i32 // the name size in bytes
 *  name: byte[n_size] // the name
 *  tag: byte // the type tag: 0 -> function, 1 -> table, 2 -> memory, 3 -> global
 *  addr: i64 // the export address
 *  }}}
 */
class Module(val address: Long) extends AnyVal {

  /** Returns the type address for the given type index in this module. */
  @inline
  def tpe(index: Int)(implicit store: Store): Type =
    new Type(store.getLong(store.getLong(address) + index * 8))

  /** Returns the function address for the given function index in this module. */
  @inline
  def func(index: Int)(implicit store: Store): Function =
    new Function(store.getLong(store.getLong(address + 8) + index * 8))

  /** Returns the table address for the given table index in this module. */
  @inline
  def table(index: Int)(implicit store: Store): Table =
    new Table(store.getLong(store.getLong(address + 16) + index * 8))

  /** Returns the memory address for the given memory index in this module. */
  @inline
  def memory(index: Int)(implicit store: Store): Memory =
    new Memory(store.getLong(store.getLong(address + 24) + index * 8))

  /** Returns the global address for the given global index in this module. */
  @inline
  def global(index: Int)(implicit store: Store): Global =
    new Global(store.getLong(store.getLong(address + 32) + index * 8))

}
