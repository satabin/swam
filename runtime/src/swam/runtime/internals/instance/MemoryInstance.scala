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

import scala.language.higherKinds

private[runtime] class MemoryInstance[F[_]](min: Int, max: Option[Int], onHeap: Boolean, hardMax: Int) extends Memory[F] {

  val tpe = MemType(Limits(min, max.map(math.min(_, hardMax))))

  var buffer = allocate(min * pageSize)

  def allocate(size: Int) =
    if (onHeap)
      ByteBuffer.allocate(size)
    else
      ByteBuffer.allocateDirect(size)

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

  def grow(by: Int): Boolean = try {
    val newSize = StrictMath.addExact(size, StrictMath.multiplyExact(by, pageSize))
    check(newSize) && doGrow(newSize)
  } catch {
    case _: ArithmeticException => false
  }

  def doGrow(size: Int): Boolean = {
    val old = buffer
    buffer = allocate(size)
    buffer.put(old)
    true
  }

  def check(size: Long): Boolean =
    max match {
      case Some(max) => size <= max * pageSize
      case None      => size <= hardMax * pageSize
    }

  def writeBytes(idx: Int, bytes: ByteBuffer): Unit = {
    buffer.position(idx)
    buffer.put(bytes)
  }

}
