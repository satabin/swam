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

import java.nio.ByteBuffer

/** A memory backed by a [[java.nio.ByteBuffer ByteBuffer]].
 */
class ByteBufferStore(capacity: Int) extends Store {

  private val buffer = ByteBuffer.allocate(capacity)

  def allocate(size: Int): Long =
    if (buffer.capacity < buffer.position + size + 4) {
      -1
    } else {
      // a block is of size `size + 4` bytes.
      // the first 4 bytes are used to register the size of the following block
      buffer.putInt(size)
      val addr = buffer.position
      buffer.position(buffer.position + size + 4)
      addr
    }

  def getLong(addr: Long): Long =
    buffer.getLong(addr.toInt)

}
