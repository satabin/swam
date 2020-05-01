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

import cats.effect._

import java.nio.{ByteBuffer, ByteOrder}

private[runtime] class MemoryInstance[F[_]](min: Int, max: Option[Int], onHeap: Boolean, hardMax: Int)(
    implicit F: Async[F])
    extends Memory[F] {

  val tpe = MemType(Limits(min, max))

  var buffer = allocate(min * pageSize)

  def allocate(size: Int) = {
    val buffer =
      if (onHeap)
        ByteBuffer.allocate(size)
      else
        ByteBuffer.allocateDirect(size)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer
  }

  def size = buffer.capacity

  def unsafeWriteByte(idx: Int, v: Byte) =
    buffer.put(idx, v): Unit

  def unsafeReadByte(idx: Int) =
    buffer.get(idx)

  def unsafeWriteShort(idx: Int, v: Short) =
    buffer.putShort(idx, v): Unit

  def unsafeReadShort(idx: Int) =
    buffer.getShort(idx)

  def unsafeWriteInt(idx: Int, v: Int) =
    buffer.putInt(idx, v): Unit

  def unsafeReadInt(idx: Int) =
    buffer.getInt(idx)

  def unsafeWriteLong(idx: Int, v: Long) =
    buffer.putLong(idx, v): Unit

  def unsafeReadLong(idx: Int) =
    buffer.getLong(idx)

  def unsafeWriteFloat(idx: Int, v: Float) =
    buffer.putFloat(idx, v): Unit

  def unsafeReadFloat(idx: Int) =
    buffer.getFloat(idx)

  def unsafeWriteDouble(idx: Int, v: Double) =
    buffer.putDouble(idx, v): Unit

  def unsafeReadDouble(idx: Int) =
    buffer.getDouble(idx)

  def unsafeGrow(by: Int) =
    try {
      val newSize = StrictMath.addExact(size, StrictMath.multiplyExact(by, pageSize))
      check(newSize) && doGrow(newSize)
    } catch {
      case _: ArithmeticException => false
    }

  def doGrow(size: Int): Boolean = {
    val old = buffer
    buffer = allocate(size)
    old.position(0)
    buffer.put(old)
    true
  }

  def check(size: Long): Boolean =
    max match {
      case Some(max) => size <= max * pageSize
      case None      => size <= hardMax * pageSize
    }

  def unsafeWriteBytes(idx: Int, bytes: ByteBuffer) = {
    bytes.mark()
    buffer.position(idx)
    buffer.put(bytes)
    bytes.reset(): Unit
  }

  def unsafeReadBytes(idx: Int, length: Int) = {
    val res = Array.ofDim[Byte](length)
    buffer.position(idx)
    buffer.get(res, 0, length)
    res
  }

}
