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
import cats.implicits._

import shapeless._
import shapeless.ops.function.FnToProduct

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

  implicit def pureFunctionAsInterface[F[_], Fn, Params, Ret](implicit F: Monad[F],
                                                              fn: FnToProduct.Aux[Fn, Params => Ret],
                                                              params: ValuesReader[F, Params],
                                                              ret: ValuesWriter[F, Ret]): AsInterface[Fn, F] =
    new AsInterface[Fn, F] {
      def view(f: Fn): Interface[F, Type] =
        new Function[F] {
          def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Vector[Value]] =
            for {
              params <- params.read(parameters, m)
              rawRes <- F.pure(fn(f)(params))
              res <- ret.write(rawRes, m)
            } yield res
          def tpe: FuncType =
            FuncType(params.swamTypes, ret.swamTypes)
        }
    }

  implicit def functionAsInterface[F[_], Fn, Params, Ret](implicit F: Monad[F],
                                                          fn: FnToProduct.Aux[Fn, Params => F[Ret]],
                                                          params: ValuesReader[F, Params],
                                                          ret: ValuesWriter[F, Ret]): AsInterface[Fn, F] =
    new AsInterface[Fn, F] {
      def view(f: Fn): Interface[F, Type] =
        new Function[F] {
          def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Vector[Value]] =
            for {
              params <- params.read(parameters, m)
              rawRes <- fn(f)(params)
              res <- ret.write(rawRes, m)
            } yield res
          def tpe: FuncType =
            FuncType(params.swamTypes, ret.swamTypes)
        }
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
        def unsafeWriteByte(idx: Int, v: Byte) = b.put(idx, v): Unit
        def unsafeWriteBytes(idx: Int, bytes: ByteBuffer) = {
          bytes.mark()
          b.position(idx)
          b.put(bytes)
          bytes.reset(): Unit
        }
        def unsafeReadBytes(idx: Int, length: Int) = {
          val res = Array.ofDim[Byte](length)
          b.position(idx)
          b.get(res, 0, length)
          res
        }
        def unsafeWriteDouble(idx: Int, v: Double) = b.putDouble(idx, v): Unit
        def unsafeWriteFloat(idx: Int, v: Float) = b.putFloat(idx, v): Unit
        def unsafeWriteInt(idx: Int, v: Int) = b.putInt(idx, v): Unit
        def unsafeWriteLong(idx: Int, v: Long) = b.putLong(idx, v): Unit
        def unsafeWriteShort(idx: Int, v: Short) = b.putShort(idx, v): Unit
      }
    }

}
