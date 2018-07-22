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

import scodec.bits._

/** The runtime representation of a loaded module.
  */
case class Module(exports: Vector[Export], imports: Vector[Import], customs: Vector[Custom])

sealed trait Import {
  val moduleName: String
  val fieldName: String
}
object Import {
  case class Function(moduleName: String, fieldName: String, tpe: FuncType) extends Import
  case class Table(moduleName: String, fieldName: String, tpe: TableType) extends Import
  case class Memory(moduleName: String, fieldName: String, tpe: MemType) extends Import
  case class Global(moduleName: String, fieldName: String, tpe: GlobalType) extends Import
}

sealed trait Export {
  val fieldName: String
}
object Export {
  case class Function(fieldName: String, tpe: FuncType) extends Export
  case class Table(fieldName: String, tpe: TableType) extends Export
  case class Memory(fieldName: String, tpe: MemType) extends Export
  case class Global(fieldName: String, tpe: GlobalType) extends Export
}

case class Custom(name: String, payload: BitVector)
