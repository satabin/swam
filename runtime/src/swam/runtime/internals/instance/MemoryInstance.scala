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

    tracer.traceEvent("msize", size)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer
  }

  def size = buffer.capacity

  def unsafeWriteByte(idx: Int, v: Byte) = {
    tracer.traceEvent("mwrite", "i8", idx, v)
    buffer.put(idx, v)
  }

  def unsafeReadByte(idx: Int) = {
    val r = buffer.get(idx)
    tracer.traceEvent("mread", "i8", idx, r)
    r
  }

  def unsafeWriteShort(idx: Int, v: Short) = {
    buffer.putShort(idx, v)
    tracer.traceEvent("mwrite", "i16", idx, v)
  }

  def unsafeReadShort(idx: Int) = {
    val r = buffer.getShort(idx)
    tracer.traceEvent("mread", "i16", idx, r)
    r
  }

  def unsafeWriteInt(idx: Int, v: Int) = {
    buffer.putInt(idx, v)
    tracer.traceEvent("mwrite", "i32", idx, v)
  }

  def unsafeReadInt(idx: Int) = {
    val r = buffer.getInt(idx)
    tracer.traceEvent("mwrite", "i32", idx, r)
    r
  }

  def unsafeWriteLong(idx: Int, v: Long) = {
    buffer.putLong(idx, v)
    tracer.traceEvent("mwrite", "i64", idx, v)
  }

  def unsafeReadLong(idx: Int) = {
    val r = buffer.getLong(idx)
    tracer.traceEvent("mread", "i32", idx, r)
    r
  }

  def unsafeWriteFloat(idx: Int, v: Float) = {
    buffer.putFloat(idx, v)
    tracer.traceEvent("mwrite", "f32", idx, v)
  }

  def unsafeReadFloat(idx: Int) = {
    val r = buffer.getFloat(idx)
    tracer.traceEvent("mread", "f32", idx, r)
    r
  }

  def unsafeWriteDouble(idx: Int, v: Double) = {
    buffer.putDouble(idx, v)
    tracer.traceEvent("mwrite", "f64", idx, v)
  }

  def unsafeReadDouble(idx: Int) = {
    val r = buffer.getDouble(idx)
    tracer.traceEvent("mread", "f64", idx, r)
    r
  }

  def unsafeGrow(by: Int) =
    try {
      val newSize = StrictMath.addExact(size, StrictMath.multiplyExact(by, pageSize))
      check(newSize) && doGrow(newSize)
    } catch {
      case _: ArithmeticException => false
    }

  def doGrow(size: Int): Boolean = {

    tracer.traceEvent("mgrow", size)
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
    bytes.reset()
  }

}
