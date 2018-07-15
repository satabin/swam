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

package fastparse
package ext

import fastparse.utils.Utils._
import fastparse.core.{ParseCtx, Parsed, Parser, Precedence}
import fastparse.utils.{ElemSetHelper, Generator, ReprOps, Utils}

case class StringInV[V](strings: Map[String, V])(implicit repr: ReprOps[Char, String],
                                                 helper: ElemSetHelper[Char],
                                                 ordering: Ordering[Char])
    extends Parser[V, Char, String] {

  private[this] val trie = new TrieNode[Char](strings.keySet.toVector.map(repr.toArray(_): IndexedSeq[Char]), false)

  def parseRec(cfg: ParseCtx[Char, String], index: Int) = {
    val length = trie.query(cfg.input, index)
    if (length != -1)
      success(cfg.success, strings(cfg.input.slice(index, index + length + 1)), index + length + 1, Set.empty, false)
    else
      fail(cfg.failure, index)
  }
  override def toString = {
    s"StringInV(${strings.keySet.map(repr.literalize).mkString(", ")})"
  }
}
