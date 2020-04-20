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
package imports

import formats._

import cats._
import cats.effect._

import shapeless._

import java.nio.{ByteBuffer, ByteOrder}

/** Represents the imported elements of an instance.
  *  These can either be other module [[Instance]]s or Scala
  *  functions and variables made available to interact between
  *  both worlds.
  */
trait Imports[F[_]] {

  def find(module: String, field: String): F[Interface[F, Type]]

  def updated[T](module: String, m: T)(implicit I: AsInstance[T, F]): Imports[F]

}

private class TCImports[F[_]](imports: TCMap[String, AsInstance[?, F]])(implicit F: MonadError[F, Throwable])
    extends Imports[F] {

  def find(module: String, field: String): F[Interface[F, Type]] =
    imports.get(module) match {
      case Some(elem) => elem.typeclass.find(elem.value, field)
      case None =>
        F.raiseError(new LinkException(s"Unknown module $module"))
    }

  def updated[T](module: String, m: T)(implicit I: AsInstance[T, F]): Imports[F] =
    new TCImports(imports.updated(module, m))

}

object Imports {

  def apply[F[_]](imported: (String, Elem[AsInstance[?, F]])*)(implicit F: MonadError[F, Throwable]): Imports[F] =
    new TCImports[F](TCMap[String, AsInstance[?, F]](imported: _*))

  def apply[F[_]](imported: TCMap[String, AsInstance[?, F]])(implicit F: MonadError[F, Throwable]): Imports[F] =
    new TCImports[F](imported)

}

/** A typeclass that describes what it means for an imported object to be viewed as
  *  an instance from the engine.
  */
trait AsInstance[T, F[_]] {

  def find(t: T, field: String): F[Interface[F, Type]]

}

object AsInstance {

  implicit def fromPair[F[_], T](implicit T: AsInterface[T, F],
                                 F: MonadError[F, Throwable]): AsInstance[(String, T), F] =
    new AsInstance[(String, T), F] {
      def find(t: (String, T), field: String): F[Interface[F, Type]] =
        t match {
          case (`field`, t) => F.pure(T.view(t))
          case _            => F.raiseError(new LinkException(s"Unknown field $field"))
        }
    }

  implicit def hconsAsInstance[F[_], T, L <: HList](implicit T: AsInterface[T, F],
                                                    F: MonadError[F, Throwable],
                                                    L: AsInstance[L, F]): AsInstance[(String, T) :: L, F] =
    new AsInstance[(String, T) :: L, F] {
      def find(h: (String, T) :: L, field: String): F[Interface[F, Type]] =
        h match {
          case (`field`, t) :: _ => F.pure(T.view(t))
          case _ :: rest         => L.find(rest, field)
        }
    }

  implicit def hnilAsInstance[F[_]](implicit F: MonadError[F, Throwable]): AsInstance[HNil, F] =
    new AsInstance[HNil, F] {
      def find(h: HNil, field: String): F[Interface[F, Type]] =
        F.raiseError(new LinkException(s"Unknown field $field"))
    }

  implicit def tcMapAsInstance[F[_]](
      implicit F: MonadError[F, Throwable]): AsInstance[TCMap[String, AsInterface[?, F]], F] =
    new AsInstance[TCMap[String, AsInterface[?, F]], F] {
      def find(m: TCMap[String, AsInterface[?, F]], field: String): F[Interface[F, Type]] =
        m.get(field) match {
          case Some(elem) => F.pure(elem.typeclass.view(elem.value))
          case None       => F.raiseError(new LinkException(s"Unknown field $field"))
        }
    }

  implicit def instanceAsInstance[F[_]](implicit F: MonadError[F, Throwable]): AsInstance[Instance[F], F] =
    new AsInstance[Instance[F], F] {
      def find(i: Instance[F], field: String): F[Interface[F, Type]] =
        i.exports.field(field)
    }

}

/** A typeclass that describes what it means for a type to be viewed as
  *  an interface element from the engine.
  */
trait AsInterface[T, F[_]] {

  def view(v: T): Interface[F, Type]

}

object AsInterface {

  implicit def interfaceAsInterface[T <: Interface[F, Type], F[_]]: AsInterface[T, F] =
    new AsInterface[T, F] {
      def view(i: T) = i
    }

  implicit def valueAsInterface[T, F[_]](implicit writer: SimpleValueWriter[F, T],
                                         F: MonadError[F, Throwable]): AsInterface[T, F] =
    new AsInterface[T, F] {
      def view(t: T): Global[F] =
        new Global[F] {
          val tpe = GlobalType(writer.swamType, Mut.Const)
          def get = writer.write(t)
          def set(v: Value) =
            F.raiseError(new RuntimeException("Unable to set immutable global"))
        }
    }

  implicit def procedure0AsInterface[F[_]](implicit F: MonadError[F, Throwable]): AsInterface[() => F[Unit], F] =
    new AsInterface[() => F[Unit], F] {
      def view(f: () => F[Unit]) = new IFunction0Unit[F](f)
    }

  implicit def function0AsInsterface[Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                writer: ValueWriter[F, Ret]): AsInterface[() => F[Ret], F] =
    new AsInterface[() => F[Ret], F] {
      def view(f: () => F[Ret]) = new IFunction0(f)
    }

  implicit def procedure1AsInterface[P1, F[_]](implicit F: MonadError[F, Throwable],
                                               reader1: ValueReader[F, P1]): AsInterface[(P1) => F[Unit], F] =
    new AsInterface[(P1) => F[Unit], F] {
      def view(f: (P1) => F[Unit]) = new IFunction1Unit[F, P1](f)
    }

  implicit def function1AsInterface[P1, Ret, F[_]](implicit F: MonadError[F, Throwable],
                                                   reader1: ValueReader[F, P1],
                                                   writer: ValueWriter[F, Ret]): AsInterface[(P1) => F[Ret], F] =
    new AsInterface[(P1) => F[Ret], F] {
      def view(f: (P1) => F[Ret]) = new IFunction1[F, P1, Ret](f)
    }

  implicit def procedure2AsInterface[P1, P2, F[_]](implicit F: MonadError[F, Throwable],
                                                   reader1: ValueReader[F, P1],
                                                   reader2: ValueReader[F, P2]): AsInterface[(P1, P2) => F[Unit], F] =
    new AsInterface[(P1, P2) => F[Unit], F] {
      def view(f: (P1, P2) => F[Unit]) = new IFunction2Unit[F, P1, P2](f)
    }

  implicit def function2AsInterface[P1, P2, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2) => F[Ret], F] =
    new AsInterface[(P1, P2) => F[Ret], F] {
      def view(f: (P1, P2) => F[Ret]) = new IFunction2[F, P1, P2, Ret](f)
    }

  implicit def function3AsInterface[P1, P2, P3, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      reader3: ValueReader[F, P3],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2, P3) => F[Ret], F] =
    new AsInterface[(P1, P2, P3) => F[Ret], F] {
      def view(f: (P1, P2, P3) => F[Ret]) = new IFunction3[F, P1, P2, P3, Ret](f)
    }

  implicit def function4AsInterface[P1, P2, P3, P4, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      reader3: ValueReader[F, P3],
      reader4: ValueReader[F, P4],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2, P3, P4) => F[Ret], F] =
    new AsInterface[(P1, P2, P3, P4) => F[Ret], F] {
      def view(f: (P1, P2, P3, P4) => F[Ret]) = new IFunction4[F, P1, P2, P3, P4, Ret](f)
    }

  implicit def function5AsInterface[P1, P2, P3, P4, P5, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      reader3: ValueReader[F, P3],
      reader4: ValueReader[F, P4],
      reader5: ValueReader[F, P5],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2, P3, P4, P5) => F[Ret], F] =
    new AsInterface[(P1, P2, P3, P4, P5) => F[Ret], F] {
      def view(f: (P1, P2, P3, P4, P5) => F[Ret]) = new IFunction5[F, P1, P2, P3, P4, P5, Ret](f)
    }

  implicit def function6AsInterface[P1, P2, P3, P4, P5, P6, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      reader3: ValueReader[F, P3],
      reader4: ValueReader[F, P4],
      reader5: ValueReader[F, P5],
      reader6: ValueReader[F, P6],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2, P3, P4, P5, P6) => F[Ret], F] =
    new AsInterface[(P1, P2, P3, P4, P5, P6) => F[Ret], F] {
      def view(f: (P1, P2, P3, P4, P5, P6) => F[Ret]) = new IFunction6[F, P1, P2, P3, P4, P5, P6, Ret](f)
    }

  implicit def function7AsInterface[P1, P2, P3, P4, P5, P6, P7, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      reader3: ValueReader[F, P3],
      reader4: ValueReader[F, P4],
      reader5: ValueReader[F, P5],
      reader6: ValueReader[F, P6],
      reader7: ValueReader[F, P7],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2, P3, P4, P5, P6, P7) => F[Ret], F] =
    new AsInterface[(P1, P2, P3, P4, P5, P6, P7) => F[Ret], F] {
      def view(f: (P1, P2, P3, P4, P5, P6, P7) => F[Ret]) = new IFunction7[F, P1, P2, P3, P4, P5, P6, P7, Ret](f)
    }

  implicit def function8AsInterface[P1, P2, P3, P4, P5, P6, P7, P8, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      reader3: ValueReader[F, P3],
      reader4: ValueReader[F, P4],
      reader5: ValueReader[F, P5],
      reader6: ValueReader[F, P6],
      reader7: ValueReader[F, P7],
      reader8: ValueReader[F, P8],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2, P3, P4, P5, P6, P7, P8) => F[Ret], F] =
    new AsInterface[(P1, P2, P3, P4, P5, P6, P7, P8) => F[Ret], F] {
      def view(f: (P1, P2, P3, P4, P5, P6, P7, P8) => F[Ret]) =
        new IFunction8[F, P1, P2, P3, P4, P5, P6, P7, P8, Ret](f)
    }

  implicit def function9AsInterface[P1, P2, P3, P4, P5, P6, P7, P8, P9, Ret, F[_]](
      implicit F: MonadError[F, Throwable],
      reader1: ValueReader[F, P1],
      reader2: ValueReader[F, P2],
      reader3: ValueReader[F, P3],
      reader4: ValueReader[F, P4],
      reader5: ValueReader[F, P5],
      reader6: ValueReader[F, P6],
      reader7: ValueReader[F, P7],
      reader8: ValueReader[F, P8],
      reader9: ValueReader[F, P9],
      writer: ValueWriter[F, Ret]): AsInterface[(P1, P2, P3, P4, P5, P6, P7, P8, P9) => F[Ret], F] =
    new AsInterface[(P1, P2, P3, P4, P5, P6, P7, P8, P9) => F[Ret], F] {
      def view(f: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => F[Ret]) =
        new IFunction9[F, P1, P2, P3, P4, P5, P6, P7, P8, P9, Ret](f)
    }

  implicit def arrayAsInterface[F[_]]: AsInterface[Array[Function[F]], F] =
    new AsInterface[Array[Function[F]], F] {
      def view(a: Array[Function[F]]) = new Table[F] {
        def tpe = TableType(ElemType.FuncRef, Limits(0, Some(size)))
        def size =
          a.length
        def apply(idx: Int) =
          a(idx)
        def update(idx: Int, f: Function[F]) =
          a(idx) = f
      }
    }

  implicit def byteBufferAsInsterface[F[_]](implicit F: Async[F]): AsInterface[ByteBuffer, F] =
    new AsInterface[ByteBuffer, F] {
      def view(_b: ByteBuffer) = new Memory[F] {
        val b = _b.duplicate()
        b.order(ByteOrder.LITTLE_ENDIAN)
        def tpe: swam.MemType = MemType(Limits(b.limit() / pageSize, Some(b.capacity / pageSize)))
        def unsafeGrow(by: Int) = {
          val newSize = size + by * pageSize
          if (newSize > b.capacity) {
            false
          } else {
            b.limit(newSize)
            true
          }
        }
        def unsafeReadByte(idx: Int) = b.get(idx)
        def unsafeReadDouble(idx: Int) = b.getDouble(idx)
        def unsafeReadFloat(idx: Int) = b.getFloat(idx)
        def unsafeReadInt(idx: Int) = b.getInt(idx)
        def unsafeReadLong(idx: Int) = b.getLong(idx)
        def unsafeReadShort(idx: Int) = b.getShort(idx)
        def size = b.limit
        def unsafeWriteByte(idx: Int, v: Byte) = b.put(idx, v)
        def unsafeWriteBytes(idx: Int, bytes: ByteBuffer) = {
          bytes.mark()
          b.position(idx)
          b.put(bytes)
          bytes.reset()
        }
        def unsafeWriteDouble(idx: Int, v: Double) = b.putDouble(idx, v)
        def unsafeWriteFloat(idx: Int, v: Float) = b.putFloat(idx, v)
        def unsafeWriteInt(idx: Int, v: Int) = b.putInt(idx, v)
        def unsafeWriteLong(idx: Int, v: Long) = b.putLong(idx, v)
        def unsafeWriteShort(idx: Int, v: Short) = b.putShort(idx, v)
        def unsafeReadBytes(idx: Int, dst: Array[Byte]): Unit = {
          val old = b.position()
          b.position(idx)
          b.get(dst)
          b.position(old)
        }
      }
    }

}
