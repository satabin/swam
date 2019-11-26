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
package config

import util._

import enumeratum._
import enumeratum.EnumEntry._

/** Holds all the configurable values.
  *
  * @param stack Configures how the stack behaves.
  * @param data Configures how the data part behaves.
  */
case class EngineConfiguration(compiler: CompilerConfiguration, stack: StackConfiguration, data: DataConfiguration)

case class CompilerConfiguration(byteOrder: ConfiguredByteOrder)

sealed trait ConfiguredByteOrder extends EnumEntry with Hyphencase

object ConfiguredByteOrder extends Enum[ConfiguredByteOrder] {
  case object LittleEndian extends ConfiguredByteOrder
  case object BigEndian extends ConfiguredByteOrder
  case object Native extends ConfiguredByteOrder
  def values = findValues
}

case class DataConfiguration(onHeap: Boolean, hardMax: MemorySize)

case class StackConfiguration(size: MemorySize)
