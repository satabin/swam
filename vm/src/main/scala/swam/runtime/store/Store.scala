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

package swam.runtime.store

/** An abstract representation of the VM memory.
 *  This trait exposes all memory management function a memory
 *  back end must implement to ensure the VM behaves correctly.
 *  Please read carefully the documentation if you intend to add new back ends.
 */
trait Store {

  /** Allocates a new memory block of the required size in bytes.
   *  If no block could be created of the required size, then returns `-1`,
   *  otherwise returns the block address.
   *
   *  If a non-negative value is returned, it is guaranteed that the block size
   *  is available to write.
   */
  def allocate(size: Int): Long

  /** Returns the long at the given address.
   *  It assumes the address is correct. If it is not the case, an exception
   *  is thrown.
   */
  def getLong(addr: Long): Long

}
