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
package text

import util._

import java.nio.file.Path

import cats.effect._

import scala.collection.Searching._
import scala.collection.immutable.VectorBuilder

import scala.io.Source

/** Loads the content of the file and renders a [[TextFilePosition]].
  */
class WastPositioner(file: Path) extends Positioner[TextFilePosition] {

  private val lines =
    IO(Source.fromFile(file.toFile, "UTF-8").getLines.toVector).unsafeRunSync()

  private val offsets =
    lines
      .foldLeft((0, new VectorBuilder[Int])) {
        case ((offset, acc), line) =>
          (offset + line.size + 1, acc += offset)
      }
      ._2
      .result()

  // returns the index of the line containing the position
  private def findLine(pos: Int): Int =
    offsets.search(pos) match {
      case Found(i)           => i
      case InsertionPoint(ip) => ip - 1
    }

  def render(pos: Int) = {
    val line = findLine(pos)
    val column = pos - offsets(line)
    TextFilePosition(file, line + 1, column, lines(line))
  }
}

class WastCustomPositioner(doc: String, file: Path) extends Positioner[TextFilePosition] {
  private val lines =
    IO(doc.split("\n").map(_.trim).toVector).unsafeRunSync()

  private val offsets =
    lines
      .foldLeft((0, new VectorBuilder[Int])) {
        case ((offset, acc), line) =>
          (offset + line.size + 1, acc += offset)
      }
      ._2
      .result()

  def render(pos: Int) = {
    val line = findLine(pos)
    val column = pos - offsets(line)
    TextFilePosition(file, line + 1, column, lines(line))
  }

  // returns the index of the line containing the position
  private def findLine(pos: Int): Int =
    offsets.search(pos) match {
      case Found(i)           => i
      case InsertionPoint(ip) => ip - 1
  }

  def getLines() = {
    val lines_all = lines
    lines_all
  }
}


