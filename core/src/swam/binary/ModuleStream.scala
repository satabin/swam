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
package binary

import syntax._

import scodec.bits._
import scodec.stream._

/** The module streams expose way to encode and decode WebAssembly modules in
  *  binary format.
  *  Section are streamed in the order they arrive, which may not be correct when
  *  a module is parsed.
  *  It is up to the client code to check section constraints.
  */
object ModuleStream {

  val header = hex"0061736d01000000"

  val decoder: StreamDecoder[Section] =
    StreamDecoder.many(WasmCodec.section)

  val encoder: StreamEncoder[Section] =
    StreamEncoder.many(WasmCodec.section)

}
