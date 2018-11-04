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

import fs2.{io, text, Stream}

import scala.collection.Searching._

/** Loads the content of the file and renders a [[TextFilePosition]].
  */
class WastPositioner(file: Path)(implicit cs: ContextShift[IO]) extends Positioner[TextFilePosition] {

  private val lineStream =
    io.file
      .readAll[IO](file, blockingExecutionContext, 4096)
      .through(text.utf8Decode)
      .through(text.lines)

  private val lines =
    lineStream.compile.toVector
      .unsafeRunSync()

  private val offsets =
    lineStream
      .mapAccumulate(0) { (offset, line) =>
        (offset + line.size + 1, offset)
      }
      .map(_._2)
      .compile
      .toVector
      .unsafeRunSync()

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
