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
import cats.implicits._

import java.nio.{ByteBuffer, ByteOrder}

import scala.language.higherKinds

private[runtime] class MemoryInstance[F[_]](min: Int, max: Option[Int], onHeap: Boolean, hardMax: Int)(
    implicit F: Async[F], tracer: Tracer = null)
    extends Memory[F] {

  val tpe = MemType(Limits(min, max))

  var buffer = allocate(min * pageSize)

  def allocate(size: Int) = {
    val buffer =
      if (onHeap)
        ByteBuffer.allocate(size)
      else
        ByteBuffer.allocateDirect(size)

    if (tracer != null)tracer.traceEvent("msize", size)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer
  }

  def size = buffer.capacity

  def writeByte(idx: Int, v: Byte) = {
    if (tracer != null)tracer.traceEvent("msize", size)
    F.delay(buffer.put(idx, v))
  }

  def readByte(idx: Int) = {
    val result = F.delay(buffer.get(idx))
    // mread, address, size (1 byte)
    if (tracer != null)tracer.traceEvent("mread", idx, 1)
    result
  }

  def writeShort(idx: Int, v: Short) = {

    if (tracer != null)tracer.traceEvent("mwrite", idx, 2, v)
    F.delay(buffer.putShort(idx, v))
  }

  def readShort(idx: Int) = {
    if (tracer != null)tracer.traceEvent("mread", idx, 2)
    F.delay(buffer.getShort(idx))
  }

  def writeInt(idx: Int, v: Int) = {
    if (tracer != null)tracer.traceEvent("mwrite", idx, 4)
    F.delay(buffer.putInt(idx, v))
  }

  def readInt(idx: Int) = {
    if (tracer != null)tracer.traceEvent("mread", idx, 4)
    F.delay(buffer.getInt(idx))
  }

  def writeLong(idx: Int, v: Long) = {
    if (tracer != null)tracer.traceEvent("mwrite", idx, 8)
    F.delay(buffer.putLong(idx, v))
  }

  def readLong(idx: Int) = {
    if (tracer != null)tracer.traceEvent("mread", idx, 8)
    F.delay(buffer.getLong(idx))
  }

  def writeFloat(idx: Int, v: Float) = {
    if (tracer != null)tracer.traceEvent("mwriteF", idx, 4)
    F.delay(buffer.putFloat(idx, v))
  }

  def readFloat(idx: Int) = {
    if (tracer != null)tracer.traceEvent("mreadF", idx, 4)
    F.delay(buffer.getFloat(idx))
  }

  def writeDouble(idx: Int, v: Double) = {
    if (tracer != null)tracer.traceEvent("mwriteF", idx, 8)
    F.delay(buffer.putDouble(idx, v))
  }

  def readDouble(idx: Int) = {
    if (tracer != null)tracer.traceEvent("mreadF", idx, 8)
    F.delay(buffer.getDouble(idx))
  }

  def grow(by: Int) =
    F.delay {
      val newSize = StrictMath.addExact(size, StrictMath.multiplyExact(by, pageSize))
      check(newSize) && doGrow(newSize)
    } recover {
      case _: ArithmeticException => false
    }

  def doGrow(size: Int): Boolean = {

    if (tracer != null)tracer.traceEvent("mgrow", size)
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

  def writeBytes(idx: Int, bytes: ByteBuffer) = F.delay {

    if (tracer != null)tracer.traceEvent("mwriteBuffer", idx)
    buffer.position(idx)
    buffer.put(bytes)
  }

}
