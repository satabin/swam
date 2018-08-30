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
package compiler

case class Context(types: Vector[FuncType] = Vector.empty,
                   funcs: Vector[Int] = Vector.empty,
                   code: Vector[Func] = Vector.empty,
                   tables: Vector[Tab] = Vector.empty,
                   mems: Vector[Mem] = Vector.empty,
                   globals: Vector[Glob] = Vector.empty,
                   elems: Vector[CompiledElem] = Vector.empty,
                   data: Vector[CompiledData] = Vector.empty,
                   start: Option[Int] = None,
                   exports: Vector[runtime.Export] = Vector.empty,
                   imports: Vector[runtime.Import] = Vector.empty,
                   customs: Vector[runtime.Custom] = Vector.empty) {
  lazy val functions: Vector[FuncType] = funcs.map(types(_))
}

sealed trait Func
object Func {
  case class Compiled(f: CompiledFunction) extends Func
  case class Imported(tpe: FuncType) extends Func
}

sealed trait Glob {
  val tpe: GlobalType
}
object Glob {
  case class Compiled(f: CompiledGlobal) extends Glob {
    val tpe = f.tpe
  }
  case class Imported(tpe: GlobalType) extends Glob
}

sealed trait Tab {
  val tpe: TableType
}
object Tab {
  case class Compiled(tpe: TableType) extends Tab
  case class Imported(tpe: TableType) extends Tab
}

sealed trait Mem {
  val tpe: MemType
}
object Mem {
  case class Compiled(tpe: MemType) extends Mem
  case class Imported(tpe: MemType) extends Mem
}
