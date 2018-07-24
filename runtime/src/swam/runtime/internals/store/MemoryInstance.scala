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

package swam.runtime.internals.store

abstract class MemoryInstance {
  def size: Int
  def grow(by: Int): Boolean
  def readByte(idx: Int): Byte
  def readShort(idx: Int): Short
  def readInt(idx: Int): Int
  def readLong(idx: Int): Long
  def readFloat(idx: Int): Float
  def readDouble(idx: Int): Double
  def writeByte(idx: Int, v: Byte): Unit
  def writeShort(idx: Int, v: Short): Unit
  def writeInt(idx: Int, v: Int): Unit
  def writeLong(idx: Int, v: Long): Unit
  def writeFloat(idx: Int, v: Float): Unit
  def writeDouble(idx: Int, v: Double): Unit
}
