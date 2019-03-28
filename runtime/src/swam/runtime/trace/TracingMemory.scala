/*
 * Copyright 2019 Lucas Satabin
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
package trace

import cats._
import java.nio.ByteBuffer

/** A memory instance that traces all calls to the underlying memory instance. */
private[runtime] class TracingMemory[F[_]](inner: Memory[F], tracer: Tracer)(implicit F: MonadError[F, Throwable])
    extends Memory[F] {

  def tpe: MemType = inner.tpe

  def size: Int = {
    val res = inner.size
    tracer.traceEvent(EventType.MSize, List(res.toString))
    res
  }

  def unsafeGrow(by: Int): Boolean = {
    tracer.traceEvent(EventType.MGrow, List(by.toString))
    inner.unsafeGrow(by)
  }

  def unsafeWriteBytes(idx: Int, bytes: ByteBuffer): Unit =
    inner.unsafeWriteBytes(idx, bytes)

  def unsafeReadBytes(idx: Int, length: Int): Array[Byte] =
    inner.unsafeReadBytes(idx, length)

  def unsafeWriteByte(idx: Int, v: Byte): Unit = {
    tracer.traceEvent(EventType.MWrite, List("i8", idx.toString, v.toString))
    inner.unsafeWriteByte(idx, v)
  }

  def unsafeReadByte(idx: Int): Byte = {
    val res = inner.unsafeReadByte(idx)
    tracer.traceEvent(EventType.MRead, List("mread", "i8", idx.toString, res.toString))
    res
  }

  def unsafeWriteShort(idx: Int, v: Short): Unit = {
    tracer.traceEvent(EventType.MWrite, List("i16", idx.toString, v.toString))
    inner.unsafeWriteShort(idx, v)
  }

  def unsafeReadShort(idx: Int): Short = {
    val res = inner.unsafeReadShort(idx)
    tracer.traceEvent(EventType.MRead, List("mread", "i16", idx.toString, res.toString))
    res
  }

  def unsafeWriteInt(idx: Int, v: Int): Unit = {
    tracer.traceEvent(EventType.MWrite, List("i32", idx.toString, v.toString))
    inner.unsafeWriteInt(idx, v)
  }

  def unsafeReadInt(idx: Int): Int = {
    val res = inner.unsafeReadInt(idx)
    tracer.traceEvent(EventType.MRead, List("mread", "i32", idx.toString, res.toString))
    res
  }

  def unsafeWriteLong(idx: Int, v: Long): Unit = {
    tracer.traceEvent(EventType.MWrite, List("i64", idx.toString, v.toString))
    inner.unsafeWriteLong(idx, v)
  }

  def unsafeReadLong(idx: Int): Long = {
    val res = inner.unsafeReadLong(idx)
    tracer.traceEvent(EventType.MRead, List("mread", "i64", idx.toString, res.toString))
    res
  }

  def unsafeWriteFloat(idx: Int, v: Float): Unit = {
    tracer.traceEvent(EventType.MWrite, List("f32", idx.toString, v.toString))
    inner.unsafeWriteFloat(idx, v)
  }

  def unsafeReadFloat(idx: Int): Float = {
    val res = inner.unsafeReadFloat(idx)
    tracer.traceEvent(EventType.MRead, List("mread", "f32", idx.toString, res.toString))
    res
  }

  def unsafeWriteDouble(idx: Int, v: Double): Unit = {
    tracer.traceEvent(EventType.MWrite, List("f64", idx.toString, v.toString))
    inner.unsafeWriteDouble(idx, v)
  }

  def unsafeReadDouble(idx: Int): Double = {
    val res = inner.unsafeReadDouble(idx)
    tracer.traceEvent(EventType.MRead, List("mread", "f64", idx.toString, res.toString))
    res
  }

}

private[runtime] object TracingMemory {
  def apply[F[_]](inner: Memory[F], tracer: Tracer)(implicit F: MonadError[F, Throwable]): TracingMemory[F] =
    inner match {
      case tracing: TracingMemory[F] => tracing // do not wrap already tracing instances
      case _                         => new TracingMemory(inner, tracer)
    }
}
