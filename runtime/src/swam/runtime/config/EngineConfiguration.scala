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

import squants.information._

import enumeratum._
import enumeratum.EnumEntry._

/** Holds all the
  * configurable values.
  *
  * @param useLowLevelAsm Whether engine compiles and run low-level bytecode.
  * @param stack Configures how the stack behaves.
  * @param data Configures how the data part behaves.
  */
case class EngineConfiguration(useLowLevelAsm: Boolean,
                               compiler: CompilerConfiguration,
                               stack: StackConfiguration,
                               data: DataConfiguration,
                               tracer: TraceConfiguration)

case class CompilerConfiguration(low: LowLevelCompilerConfiguration)

case class LowLevelCompilerConfiguration(byteOrder: ConfiguredByteOrder)

sealed trait ConfiguredByteOrder extends EnumEntry with Hyphencase

object ConfiguredByteOrder extends Enum[ConfiguredByteOrder] {

  def values = findValues

  case object LittleEndian extends ConfiguredByteOrder
  case object BigEndian extends ConfiguredByteOrder
  case object Native extends ConfiguredByteOrder
}

case class StackConfiguration(high: HighLevelStackConfiguration, low: LowLevelStackConfiguration)

case class DataConfiguration(onHeap: Boolean, hardMax: Information)

case class LowLevelStackConfiguration(size: Information)

case class HighLevelStackConfiguration(size: Information, callDepth: Int)

case class TraceConfiguration(handler: HandlerType,
                              filter: String,
                              level: String,
                              fileHandler: TracerFileHandlerCondiguration,
                              socketHandler: SocketHanndlerCondiguration,
                              custom: CustomTracerConfiguration)

case class TracerFileHandlerCondiguration(pattern: String, append: Boolean, folder: String);
case class SocketHanndlerCondiguration(host: String, port: Int);
case class CustomTracerConfiguration(className: String)

sealed trait HandlerType extends EnumEntry with Hyphencase

object HandlerType extends Enum[HandlerType] {

  def values = findValues

  case object Console extends HandlerType
  case object File extends HandlerType
  case object None extends HandlerType
  case object Custom extends HandlerType
}
