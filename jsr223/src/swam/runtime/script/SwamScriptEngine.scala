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
package script

import java.io.Reader

import javax.script._

import scala.beans._

class SwamScriptEngine(@BeanProperty val factory: SwamScriptEngineFactory) extends ScriptEngine {

  import SwamScriptEngine._

  def createBindings(): Bindings =
    new SimpleBindings

  /** Reads, validates, and instantiates a WebAssembly module, returning the instance.
    *
    * Tries to determine in what format the script is passed (binary or text) based on the following elements:
    *  1. if attribute key [[SwamScriptEngine#WASM_FORMAT WASM_FORMAT]] exists with a value of type [[WasmFormat]], then use it;
    *  1. else, if attribute key `FILENAME` exists with a string value, then use it as follows:
    *    - if the file name extension is `wast` or `wat`, use [[WasmFormat#TextFormat]],
    *    - otherwise, use [[WasmFormat#BinaryFormat]];
    *  1. else, if reader is of type `java.io.FileReader`, use [[WasmFormat#TextFormat]];
    *  1. otherwise, fall back to binary format.
    */
  def eval(reader: Reader, context: ScriptContext): Object = ???

  def eval(script: String, context: ScriptContext): Object = ???

  def eval(reader: Reader, bindings: Bindings): Object = ???

  def eval(script: String, bindings: Bindings): Object = ???

  def eval(reader: Reader): Object = ???

  def eval(script: String): Object = ???

  def get(key: String): Object = ???

  def getBindings(scope: Int): Bindings = ???

  def getContext(): ScriptContext = ???

  def put(key: String, value: Object): Unit = ???

  def setBindings(bindings: Bindings, scope: Int): Unit = ???

  def setContext(context: ScriptContext): Unit = ???

}

object SwamScriptEngine {

  val WASM_FORMAT = "swam.source.format"

  /** Evaluation scope is a scope that is specific to each call to an `eval` method.
    * It should not be shared between different calls to `eval`.
    */
  val EVAL_SCOPE = 0

}

sealed trait WasmFormat
object WasmFormat {
  case object BinaryFormat extends WasmFormat
  case object TextFormat extends WasmFormat
}
