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
package internals
package instance

import java.nio.ByteBuffer

private[runtime] class MemoryInstance(min: Int, max: Option[Int]) {

  var buffer = ByteBuffer.allocate(min)

  def size = buffer.capacity

  def writeByte(idx: Int, v: Byte): Unit =
    buffer.put(idx, v)

  def readByte(idx: Int): Byte =
    buffer.get(idx)

  def writeShort(idx: Int, v: Short): Unit =
    buffer.putShort(idx, v)

  def readShort(idx: Int): Short =
    buffer.getShort(idx)

  def writeInt(idx: Int, v: Int): Unit =
    buffer.putInt(idx, v)

  def readInt(idx: Int): Int =
    buffer.getInt(idx)

  def writeLong(idx: Int, v: Long): Unit =
    buffer.putLong(idx, v)

  def readLong(idx: Int): Long =
    buffer.getLong(idx)

  def writeFloat(idx: Int, v: Float): Unit =
    buffer.putFloat(idx, v)

  def readFloat(idx: Int): Float =
    buffer.getFloat(idx)

  def writeDouble(idx: Int, v: Double): Unit =
    buffer.putDouble(idx, v)

  def readDouble(idx: Int): Double =
    buffer.getDouble(idx)

  def grow(by: Int): Boolean = {
    val newSize = size + by * pageSize
    check(size) && doGrow(size)
  }

  private def doGrow(size: Int): Boolean = {
    val old = buffer
    buffer = ByteBuffer.allocate(size)
    buffer.put(old)
    true
  }

  private def check(size: Int): Boolean =
    max match {
      case Some(max) => size <= max
      case None      => true
    }

}
