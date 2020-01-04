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
import binary.custom._
import internals.compiler._

import scodec._
import scodec.bits._

import cats.effect._

/** The runtime representation of a validated and compiled module.
  *
  * @param exports Returns the ordered list of elements exported by this module.
  * @param imports Returns the ordered list of elements imported by this module.
  *                The imports are required when the module is instantiated.
  * @param customs Returns the ordered list of custom section of this module.
  *                Custom sections may be used to implement extensions or get
  *                extra information about the module.
  */
class Module[F[_]] private[runtime] (val exports: Vector[Export],
                                     val imports: Vector[Import],
                                     val customs: Vector[Custom],
                                     private[runtime] val types: Vector[FuncType],
                                     private[runtime] val engine: Engine[F],
                                     private[runtime] val globals: Vector[CompiledGlobal[F]],
                                     private[runtime] val tables: Vector[TableType],
                                     private[runtime] val memories: Vector[MemType],
                                     private[runtime] val start: Option[Int],
                                     private[runtime] val functions: Vector[CompiledFunction[F]],
                                     private[runtime] val elems: Vector[CompiledElem[F]],
                                     private[runtime] val data: Vector[CompiledData[F]])(implicit F: Effect[F]) {
  self =>

  private lazy val names = {
    val sec = customs.collectFirst {
      case Custom("name", payload) => payload
    }
    sec match {
      case Some(payload) =>
        NameSectionHandler.codec.decodeValue(payload) match {
          case Attempt.Successful(names) => Some(names)
          case _                         => None // simply ignore malformed name section
        }
      case None => None
    }
  }

  /** Returns the module name if any.
    *
    * A module has a name if it was provided by a custom name section.
    */
  def name: Option[String] = names.flatMap(_.subsections.collectFirst { case ModuleName(n) => n })

  /** Instantiates this module with no imports.
    * The returned [[Instance]] can then be used to access exported elements.
    *
    * If instantiation fails, returns an error with the message wrapped in it.
    */
  def instantiate: F[Instance[F]] =
    engine.instantiate(this, NoImports[F])

  /** A module that can be instantiated with some imports.
    * This can be reused to instantiate several times the same
    * module with the same imports.
    */
  class Instantiable private[Module] (val imports: Imports[F]) {

    /** The module from which this instantiable was created. */
    def module: Module[F] = self

    /** Adds a new import to be used when instantiating. */
    def importing[I](name: String, i: I)(implicit I: AsInstance[I, F]): Instantiable =
      new Instantiable(imports.updated(name, i))

    /** Instantiates this module with previously provide imports.
      * The returned [[Instance]] can then be used to access exported elements.
      *
      * If instantiation fails, returns an error with the message wrapped in it.
      */
    def instantiate: F[Instance[F]] =
      engine.instantiate(self, imports)
  }

  /** Adds imports to be used when instantiating. */
  def importing(imports: Imports[F]): Instantiable =
    new Instantiable(imports)

  /** Adds a new import to be used when instantiating. */
  def importing[I](name: String, i: I)(implicit I: AsInstance[I, F]): Instantiable =
    new Instantiable(Imports[F](module(name, i)))

  /** Access to filtered imported elements. */
  object imported {

    /** Returns the ordered list of imported functions. */
    def functions: Vector[Import.Function] =
      imports.collect {
        case f @ Import.Function(_, _, _, _) => f
      }

    /** Returns the ordered list of imported globals. */
    def globals: Vector[Import.Global] =
      imports.collect {
        case f @ Import.Global(_, _, _) => f
      }

    /** Returns the ordered list of imported tables. */
    def tables: Vector[Import.Table] =
      imports.collect {
        case f @ Import.Table(_, _, _) => f
      }

    /** Returns the ordered list of imported memories. */
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
