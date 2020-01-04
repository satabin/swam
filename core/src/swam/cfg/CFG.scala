/*
 * Copyright 2019 Lucas Satabin
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
package cfg

import syntax.Inst

/** An immutable control-flow graph.
  *
  * To create a new CFG, see [[CFGBuilder]].
  */
class CFG private[cfg] (basicBlocks: Array[BasicBlock]) {

  /** The basic blocks in this control flow graph. */
  def blocks: List[BasicBlock] = basicBlocks.toList

  /** The graph entry point. */
  val entry: BasicBlock = basicBlocks(0)

  /** The graph exit point. */
  val exit: BasicBlock = basicBlocks(1)

}

case class BasicBlock(name: String, stmts: List[Inst], jump: Option[Jump])
