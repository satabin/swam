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

import javax.script._

import java.io.{Reader, Writer}

import java.util.{List => JList}

class SwamScriptContext(parent: ScriptContext, engine: SwamScriptEngine) extends ScriptContext {

  private var evalScope: Bindings =
    engine.createBindings()

  def getAttribute(name: String): Object = ???

  def getAttribute(name: String, scope: Int): Object = ???

  def getAttributesScope(name: String): Int = ???

  def getBindings(scope: Int): javax.script.Bindings = ???

  def getErrorWriter(): Writer = ???

  def getReader(): Reader = ???

  def getScopes(): JList[Integer] = ???

  def getWriter(): Writer = ???

  def removeAttribute(name: String, scope: Int): Object = ???

  def setAttribute(name: String, value: Any, scope: Int): Unit = ???

  def setBindings(bindings: Bindings, scope: Int): Unit =
    if(scope == SwamScriptEngine.EVAL_SCOPE)
      evalScope = bindings
    else
      parent.setBindings(bindings, scope)

  def setErrorWriter(writer: Writer): Unit = ???

  def setReader(reader: Reader): Unit = ???

  def setWriter(writer: Writer): Unit = ???

}
