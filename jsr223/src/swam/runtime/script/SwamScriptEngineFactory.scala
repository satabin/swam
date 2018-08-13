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

import java.util.{List => JList}

import javax.script._

import scala.collection.JavaConverters._

import scala.beans._

/** JSR-223 compliant script engine factory for WebAssembly. The engine answers for:
  *
  *  - names "swam", "Swam", "wasm", "WASM", "WebAssembly", "webassembly";
  *  - MIME types "application/wasm";
  *  - as well as for the extensions "wasm", "wast", and "wat".
  *
  */
class SwamScriptEngineFactory extends ScriptEngineFactory {

  @BeanProperty
  val extensions: JList[String] =
    List("wasm", "wast", "wat").asJava

  @BeanProperty
  val mimeTypes: JList[String] =
    List("application/wasm").asJava

  @BeanProperty
  val names: JList[String] =
    List("swam", "Swam", "wasm", "WASM", "WebAssembly", "webassembly").asJava

  @BeanProperty
  val engineName: String =
    "Swam WebAssembly Engine"

  @BeanProperty
  val engineVersion: String =
    "0.1.0"

  @BeanProperty
  val languageName: String =
    "WebAssembly"

  @BeanProperty
  val languageVersion: String =
    "1.0"

  def getMethodCallSyntax(obj: String, m: String, args: String*): String =
    ???

  def getOutputStatement(toDisplay: String): String =
    ???

  def getParameter(key: String): Object =
    key match {
      case ScriptEngine.ENGINE           => engineName
      case ScriptEngine.ENGINE_VERSION   => engineVersion
      case ScriptEngine.LANGUAGE         => language
      case ScriptEngine.LANGUAGE_VERSION => languageVersion
      case ScriptEngine.NAME             => names.get(0)
      case _                             => null
    }

  def getProgram(statements: String*): String =
    s"""(module
       |(function $$f
       |  ${statements.mkString("\n  ")})
       |(start $$s))""".stripMargin

  def getScriptEngine(): ScriptEngine =
    new SwamScriptEngine(this)

}
