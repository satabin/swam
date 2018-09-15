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

import formats._

import cats._

import java.nio.{ByteBuffer, ByteOrder}

import scala.language.higherKinds

package object imports {

  def NoImports[F[_]]: Imports[F] =
    new Imports[F](TCMap.empty[String, AsInstance[?, F]])

  def module[T, F[_]](name: String, mod: T)(implicit I: AsInstance[T, F]): (String, Elem[AsInstance[?, F]]) =
    (name, Elem.fromValue[T, AsInstance[?, F]](mod))

  implicit def instanceAsInstance[F[_]]: AsInstance[Instance[F], F] =
    new AsInstance[Instance[F], F] {
      def find(i: Instance[F], field: String)(implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
        i.exports.field(field)
    }

  implicit def tcMapAsInstance[F[_]]: AsInstance[TCMap[String, AsInterface[?, F]], F] =
    new AsInstance[TCMap[String, AsInterface[?, F]], F] {
      def find(m: TCMap[String, AsInterface[?, F]], field: String)(
          implicit F: MonadError[F, Throwable]): F[Interface[F, Type]] =
        m.get(field) match {
          case Some(elem) => F.pure(elem.typeclass.view(elem.value))
          case None       => F.raiseError(new LinkException(s"Unknown field $field"))
        }
    }

  implicit def interfaceAsInterface[T <: Interface[F, Type], F[_]]: AsInterface[T, F] =
    new AsInterface[T, F] {
      def view(i: T) = i
    }

  implicit def valueAsInterface[T, F[_]](implicit writer: SimpleValueWriter[T]): AsInterface[T, F] =
    new AsInterface[T, F] {
      def view(t: T): Global[F] =
        new Global[F] {
          val tpe = GlobalType(writer.swamType, Mut.Const)
          def get = writer.write(t)
          def set(v: Value)(implicit F: MonadError[F, Throwable]) =
            F.raiseError(new RuntimeException("Unable to set immutable global"))
        }
    }

  implicit def procedure0AsInterface[F[_]](implicit F: MonadError[F, Throwable]): AsInterface[() => F[Unit], F] =
    new AsInterface[() => F[Unit], F] {
      def view(f: () => F[Unit]) = new IFunction0Unit[F](f)
    }

  implicit def function0AsInsterface[Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                writer: ValueWriter[Ret]): AsInterface[() => F[Ret], F] =
    new AsInterface[() => F[Ret], F] {
      def view(f: () => F[Ret]) = new IFunction0(f)
    }

  implicit def procedure1AsInterface[P1, F[_]](implicit F: MonadError[F, Throwable],
                                               reader1: ValueReader[P1]): AsInterface[(P1) => F[Unit], F] =
    new AsInterface[(P1) => F[Unit], F] {
      def view(f: (P1) => F[Unit]) = new IFunction1Unit[F, P1](f)
    }

  implicit def function1AsInterface[P1, Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                   reader1: ValueReader[P1],
                                                   writer: ValueWriter[Ret]): AsInterface[(P1) => F[Ret], F] =
    new AsInterface[(P1) => F[Ret], F] {
      def view(f: (P1) => F[Ret]) = new IFunction1[F, P1, Ret](f)
    }

  implicit def procedure2AsInterface[P1, P2, F[_]](implicit F: MonadError[F, Throwable],
                                                   reader1: ValueReader[P1],
                                                   reader2: ValueReader[P2]): AsInterface[(P1, P2) => F[Unit], F] =
    new AsInterface[(P1, P2) => F[Unit], F] {
      def view(f: (P1, P2) => F[Unit]) = new IFunction2Unit[F, P1, P2](f)
    }

  implicit def function2AsInterface[P1, P2, Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                       reader1: ValueReader[P1],
                                                       reader2: ValueReader[P2],
                                                       writer: ValueWriter[Ret]): AsInterface[(P1, P2) => F[Ret], F] =
    new AsInterface[(P1, P2) => F[Ret], F] {
      def view(f: (P1, P2) => F[Ret]) = new IFunction2[F, P1, P2, Ret](f)
    }

  implicit def arrayAsInterface[F[_]]: AsInterface[Array[Function[F]], F] =
    new AsInterface[Array[Function[F]], F] {
      def view(a: Array[Function[F]]) = new Table[F] {
        def tpe = TableType(ElemType.AnyFunc, Limits(0, Some(size)))
        def size =
          a.length
        def apply(idx: Int) =
          a(idx)
        def update(idx: Int, f: Function[F]) =
          a(idx) = f
      }
    }

  implicit def byteBufferAsInsterface[F[_]]: AsInterface[ByteBuffer, F] =
    new AsInterface[ByteBuffer, F] {
      def view(_b: ByteBuffer) = new Memory[F] {
        val b = _b.duplicate()
        b.order(ByteOrder.LITTLE_ENDIAN)
        def tpe: swam.MemType = MemType(Limits(b.limit / pageSize, Some(b.capacity / pageSize)))
        def grow(by: Int): Boolean = {
          val newSize = size + by * pageSize
          if (newSize > b.capacity) {
            false
          } else {
            b.limit(newSize)
            true
          }
        }
        def readByte(idx: Int): Byte = b.get(idx)
        def readDouble(idx: Int): Double = b.getDouble(idx)
        def readFloat(idx: Int): Float = b.getFloat(idx)
        def readInt(idx: Int): Int = b.getInt(idx)
        def readLong(idx: Int): Long = b.getLong(idx)
        def readShort(idx: Int): Short = b.getShort(idx)
        def size: Int = b.limit
        def writeByte(idx: Int, v: Byte): Unit = b.put(idx, v)
        def writeBytes(idx: Int, bytes: ByteBuffer): Unit = {
          b.position(idx)
          b.put(bytes)
        }
        def writeDouble(idx: Int, v: Double): Unit = b.putDouble(idx, v)
        def writeFloat(idx: Int, v: Float): Unit = b.putFloat(idx, v)
        def writeInt(idx: Int, v: Int): Unit = b.putInt(idx, v)
        def writeLong(idx: Int, v: Long): Unit = b.putLong(idx, v)
        def writeShort(idx: Int, v: Short): Unit = b.putShort(idx, v)

      }
    }

}
