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
package store

import cats._

import scala.annotation.switch

import scala.language.higherKinds

/** The store contains all modules and instances loaded in
  *  an engine instance.
  *  Part of it is store in managed memory. This is intended
  *  to make some accesses and modifications more efficient.
  */
private[runtime] class Store[F[_]](implicit F: MonadError[F, Throwable]) {

  // current choice is to divide allocated memory in two disjoint areas:
  //  - the static part where module static data are stored when a module is compiled
  //  - the instance part where the data is allocated upon module instantiation
  //  In the future, these area may be merged, if required.

  // TODO make sizes configurable

  // contains static data (function code, ...)
  val static = LeaAllocator.create(1024 * 1024)
  // contains instance data (global instances, memory instances, ...)
  val instances = LeaAllocator.create(1024 * 1024)

  class Globals(val address: Address) {
    def write(idx: Int, v: Value): Unit =
      v match {
        case Value.Int32(v) =>
          instances.write(address + (idx * 9), 0x7f)
          instances.writeInt(address + (idx * 9) + 1, v)
        case Value.Int64(v) =>
          instances.write(address + (idx * 9), 0x7e)
          instances.writeLong(address + (idx * 9) + 1, v)
        case Value.Float32(v) =>
          instances.write(address + (idx * 9), 0x7d)
          instances.writeFloat(address + (idx * 9) + 1, v)
        case Value.Float64(v) =>
          instances.write(address + (idx * 9), 0x7c)
          instances.writeDouble(address + (idx * 9) + 1, v)
      }
    def read(idx: Int): Value =
      (instances.read(address + (idx * 9)): @switch) match {
        case 0x7f => Value.Int32(instances.readInt(address + (idx * 9) + 1))
        case 0x7e => Value.Int64(instances.readLong(address + (idx * 9) + 1))
        case 0x7d => Value.Float32(instances.readFloat(address + (idx * 9) + 1))
        case 0x7c => Value.Float64(instances.readDouble(address + (idx * 9) + 1))
      }
  }

  class Memory(var address: Address) {
    def size: Int =
      instances.readInt(address + 4)
    def grow(by: Int): Boolean = {
      val newSize = size + (by * pageSize)
      val max = instances.readInt(address)
      if (max >= 0 && newSize > max) {
        // exceed max size
        false
      } else {
        val newAddress = instances.allocate(newSize)
        if (newAddress >= 0) {
          // copy data to new segment
          instances.writeInt(newAddress, max)
          instances.writeInt(newAddress + 4, newSize)
          instances.copy(address + 8, newAddress + 8, size)
          address = newAddress
          true
        } else {
          // no space left
          false
        }
      }
    }
    def readByte(idx: Int): Byte =
      instances.read(address + 8 + idx)
    def readShort(idx: Int): Short =
      instances.readShort(address + 8 + idx)
    def readInt(idx: Int): Int =
      instances.readInt(address + 8 + idx)
    def readLong(idx: Int): Long =
      instances.readLong(address + 8 + idx)
    def readFloat(idx: Int): Float =
      instances.readFloat(address + 8 + idx)
    def readDouble(idx: Int): Double =
      instances.readDouble(address + 8 + idx)
    def writeByte(idx: Int, v: Byte): Unit =
      instances.write(address + 8 + idx, v)
    def writeShort(idx: Int, v: Short): Unit =
      instances.writeShort(address + 8 + idx, v)
    def writeInt(idx: Int, v: Int): Unit =
      instances.writeInt(address + 8 + idx, v)
    def writeLong(idx: Int, v: Long): Unit =
      instances.writeLong(address + 8 + idx, v)
    def writeFloat(idx: Int, v: Float): Unit =
      instances.writeFloat(address + 8 + idx, v)
    def writeDouble(idx: Int, v: Double): Unit =
      instances.writeDouble(address + 8 + idx, v)
  }

  /** Returns the function instance at the given address. */
  def function(addr: Address): FuncInstance =
    ???

  /** Returns the table instance at the given address. */
  def table(addr: Address): TableInstance =
    ???

  private def byte2type(byte: Byte) =
    byte match {
      case 0x7f => ValType.I32
      case 0x7e => ValType.I64
      case 0x7d => ValType.F32
      case 0x7c => ValType.F64
      case _    => throw new Exception("This is a bug")
    }

  private def type2byte(tpe: ValType) =
    tpe match {
      case ValType.I32 => 0x7f
      case ValType.I64 => 0x7e
      case ValType.F32 => 0x7d
      case ValType.F64 => 0x7c
      case _           => throw new Exception("This is a bug")
    }

}
