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
package syntax

import scodec.bits.ByteVector

case class Module(types: Vector[FuncType],
                  funcs: Vector[Func],
                  tables: Vector[TableType],
                  mems: Vector[MemType],
                  globals: Vector[Global],
                  elem: Vector[Elem],
                  data: Vector[Data],
                  start: Option[FuncIdx],
                  imports: Vector[Import],
                  exports: Vector[Export]) {

  object imported {

    def funcs: Vector[FuncType] = imports.collect {
      case Import.Function(_, _, idx) => types(idx)
    }

    def tables: Vector[TableType] = imports.collect {
      case Import.Table(_, _, tpe) => tpe
    }

    def mems: Vector[MemType] = imports.collect {
      case Import.Memory(_, _, tpe) => tpe
    }

    def globals: Vector[GlobalType] = imports.collect {
      case Import.Global(_, _, tpe) => tpe
    }

  }

}

object Module {
  val empty: Module =
    Module(Vector.empty,
           Vector.empty,
           Vector.empty,
           Vector.empty,
           Vector.empty,
           Vector.empty,
           Vector.empty,
           None,
           Vector.empty,
           Vector.empty)
}

case class Func(tpe: TypeIdx, locals: Vector[ValType], body: Expr)
