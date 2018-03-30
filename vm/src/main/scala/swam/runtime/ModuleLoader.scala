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

import syntax.Module

/** The module loader is responsible for loading the static
 *  structure of a module by its name.
 *
 *  Module loading can be made from file system, from a resource, from an URL, etc.
 *  The loader is used by the VM to locate and then instantiate a module.
 */
trait ModuleLoader {

  /** Loads and returns the module representation by its name.
   *
   *  @throws ModuleNotFoundException if the module name does not identify any module
   */
  def load(name: String): Module

}
