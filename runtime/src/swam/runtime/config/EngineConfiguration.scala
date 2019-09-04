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

/** Holds all the configurable values.
  *
  * @param useLowLevelAsm Whether engine compiles and run low-level bytecode.
  * @param stack Configures how the stack behaves.
  * @param data Configures how the data part behaves.
  */
case class EngineConfiguration(useLowLevelAsm: Boolean, stack: StackConfiguration, data: DataConfiguration, tracer: TraceConfiguration)

case class StackConfiguration(size: Information, callDepth: Int)

case class DataConfiguration(onHeap: Boolean, hardMax: Information)

// Tracer class name and event filter in regular expression format
case class TraceConfiguration(tracerName: String, filter: String, var path: String)