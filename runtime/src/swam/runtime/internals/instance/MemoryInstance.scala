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
    F.delay({
      val r = buffer.get(idx)
      if (tracer != null)tracer.traceEvent("mread", "i8", idx, r)
      r
    })
  }

  def writeShort(idx: Int, v: Short) = {
    if (tracer != null)tracer.traceEvent("mwrite", "i16", idx, v)
    F.delay(buffer.putShort(idx, v))
  }

  def readShort(idx: Int) = {
    F.delay({
      val r=  buffer.getShort(idx)
      if (tracer != null)tracer.traceEvent("mread", "i16", idx, r)
      r
    })
  }

  def writeInt(idx: Int, v: Int) = {
    if (tracer != null)tracer.traceEvent("mwrite", "i32", idx, v)
    F.delay(buffer.putInt(idx, v))
  }

  def readInt(idx: Int) = {
    F.delay({
      val r = buffer.getInt(idx)
      if (tracer != null)tracer.traceEvent("mread", "i32", idx, r)
      r
    })
  }

  def writeLong(idx: Int, v: Long) = {
    if (tracer != null)tracer.traceEvent("mwrite", "i64",  idx, v)
    F.delay(buffer.putLong(idx, v))
  }

  def readLong(idx: Int) = {
    F.delay({
      val r  = buffer.getLong(idx)
      if (tracer != null)tracer.traceEvent("mread", "i64", idx, r)
      r
    })
  }

  def writeFloat(idx: Int, v: Float) = {
    if (tracer != null)tracer.traceEvent("mwrite", "f32", idx, v)
    F.delay(buffer.putFloat(idx, v))
  }

  def readFloat(idx: Int) = {
    F.delay({
      val r = buffer.getFloat(idx)
      if (tracer != null)tracer.traceEvent("mread", "f32", idx, r)
      r
    })
  }

  def writeDouble(idx: Int, v: Double) = {
    if (tracer != null)tracer.traceEvent("mwrite", "f64", idx, v)
    F.delay(buffer.putDouble(idx, v))
  }

  def readDouble(idx: Int) = {
    F.delay({
      val r  = buffer.getDouble(idx)
      if (tracer != null)tracer.traceEvent("mread", "f64", idx, r)
      r
    })
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

    if (tracer != null)tracer.traceEvent("mwrite", "bytes", bytes.remaining(), idx)
    buffer.position(idx)
    buffer.put(bytes)
  }

}
