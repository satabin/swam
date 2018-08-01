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

import imports._
import internals.compiler._

import scodec.bits._

import scala.language.higherKinds

import java.nio.ByteBuffer

/** The runtime representation of a loaded module.
  *
  */
class Module[F[_]](val exports: Vector[Export],
                   val imports: Vector[Import],
                   val customs: Vector[Custom],
                   private[runtime] val types: Vector[FuncType],
                   private[runtime] val engine: SwamEngine[F],
                   private[runtime] val globals: Vector[CompiledGlobal],
                   private[runtime] val tables: Vector[TableType],
                   private[runtime] val memories: Vector[MemType],
                   private[runtime] val start: Option[Int],
                   private[runtime] val functions: Vector[CompiledFunction[F]],
                   private[runtime] val elems: Vector[CompiledElem],
                   private[runtime] val data: Vector[CompiledData]) {

  def newInstance(): F[Instance[F]] =
    engine.instantiate(this, Map.empty[String, Map[String, Interface[F, Type]]])

  def newInstance[I](imports: I)(implicit I: Imports[I, F]): F[Instance[F]] =
    engine.instantiate(this, imports)

  object imported {

    def functions: Vector[Import.Function] =
      imports.collect {
        case f @ Import.Function(_, _, _, _) => f
      }

    def globals: Vector[Import.Global] =
      imports.collect {
        case f @ Import.Global(_, _, _) => f
      }

    def tables: Vector[Import.Table] =
      imports.collect {
        case f @ Import.Table(_, _, _) => f
      }

    def memories: Vector[Import.Memory] =
      imports.collect {
        case f @ Import.Memory(_, _, _) => f
      }

  }

}

sealed trait Import {
  val moduleName: String
  val fieldName: String
  val tpe: Type
}
object Import {
  case class Function(moduleName: String, fieldName: String, tpeidx: Int, tpe: FuncType) extends Import
  case class Table(moduleName: String, fieldName: String, tpe: TableType) extends Import
  case class Memory(moduleName: String, fieldName: String, tpe: MemType) extends Import
  case class Global(moduleName: String, fieldName: String, tpe: GlobalType) extends Import
}

sealed trait Export {
  val fieldName: String
}
object Export {
  case class Function(fieldName: String, tpe: FuncType, idx: Int) extends Export
  case class Table(fieldName: String, tpe: TableType, idx: Int) extends Export
  case class Memory(fieldName: String, tpe: MemType, idx: Int) extends Export
  case class Global(fieldName: String, tpe: GlobalType, idx: Int) extends Export
}

case class Custom(name: String, payload: BitVector)
