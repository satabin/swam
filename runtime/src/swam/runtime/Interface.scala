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

import java.nio.ByteBuffer

import scala.language.higherKinds

sealed trait Interface[F[_], +T <: Type] {
  def tpe: T
}

trait Function[F[_]] extends Interface[F, FuncType] {
  def invoke(parameters: Vector[Value]): F[Option[Value]]
}

object Function {
  def unapply[F[_]](f: Function[F]): Option[FuncType] =
    Some(f.tpe)
}

trait Global[F[_]] extends Interface[F, GlobalType] {
  def get: Value
  def set(v: Value): Unit
}

trait Table[F[_]] extends Interface[F, TableType] {
  def size: Int
  def apply(idx: Int): Function[F]
  def update(idx: Int, f: Function[F]): Unit
}

trait Memory[F[_]] extends Interface[F, MemType] {
  def size: Int
  def grow(by: Int): Boolean
  def writeByte(idx: Int, v: Byte): Unit
  def readByte(idx: Int): Byte
  def writeShort(idx: Int, v: Short): Unit
  def readShort(idx: Int): Short
  def writeInt(idx: Int, v: Int): Unit
  def readInt(idx: Int): Int
  def writeLong(idx: Int, v: Long): Unit
  def readLong(idx: Int): Long
  def writeFloat(idx: Int, v: Float): Unit
  def readFloat(idx: Int): Float
  def writeDouble(idx: Int, v: Double): Unit
  def readDouble(idx: Int): Double
  def writeBytes(idx: Int, bytes: ByteBuffer): Unit
}
