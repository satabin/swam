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

package swam.util.pretty

import scala.annotation.tailrec

/** A pretty-printed document that can then be rendered to a certain witdh.
  */
sealed trait Doc {

  def ++(that: Doc): Doc =
    (this, that) match {
      case (Empty, Empty) => Empty
      case (Empty, _)     => that
      case (_, Empty)     => this
      case _              => Append(this, that)
    }

  private def spaces[State](outs: (State, String) => State, state: State, i: Int): State =
    outs(state, " " * i)

  private def nlspace[State](outs: (State, String) => State, state: State, i: Int): State =
    outs(state, "\n" + " " * i)

  def render(width: Int): String = {
    val out = new StringBuilder
    def r(s: String): Unit =
      out.append(s): Unit
    render(width, r _)
    out.toString
  }

  def render(width: Int, outs: String => Unit): Unit =
    render[Unit](width, (_, s) => outs(s), ())

  def render[State](width: Int, outs: (State, String) => State, initial: State): State = {
    @tailrec
    def be(state: State, used: Int, s: Seq[(Int, Mode, Doc)]): State =
      s match {
        case Seq() => state
        case Seq((indent, mode, doc), rest @ _*) =>
          doc match {
            case Empty          => be(state, used, rest)
            case Append(d1, d2) => be(state, used, (indent, mode, d1) +: (indent, mode, d2) +: rest)
            case Nest(j, d)     => be(state, used, (indent + j, mode, d) +: rest)
            case Text(t) =>
              val state1 = outs(state, t)
              be(state1, used + t.size, rest)
            case Newline =>
              val state1 = nlspace(outs, state, indent)
              be(state1, indent, rest)
            case Break(sp, off) =>
              mode match {
                case Mode.Flat =>
                  val state1 = spaces(outs, state, sp)
                  be(state1, used + sp, rest)
                case Mode.Break =>
                  val state1 = nlspace(outs, state, indent + off)
                  be(state1, indent + off, rest)
              }
            case Group(d) =>
              mode match {
                case Mode.Flat =>
                  be(state, used, (indent, Mode.Flat, d) +: rest)
                case Mode.Break =>
                  val flat = (indent, Mode.Flat, d) +: rest
                  if (fitting(flat, width - used))
                    be(state, used, flat)
                  else
                    be(state, used, (indent, Mode.Break, d) +: rest)
              }
          }
      }
    be(initial, 0, Seq((0, Mode.Break, this)))
  }

  @tailrec
  private def fitting(s: Seq[(Int, Mode, Doc)], left: Int): Boolean =
    s match {
      case Seq() => true
      case Seq((i, mode, doc), rest @ _*) =>
        if (left >= 0)
          doc match {
            case Empty          => fitting(rest, left)
            case Append(d1, d2) => fitting((i, mode, d1) +: (i, mode, d2) +: rest, left)
            case Nest(j, d)     => fitting((i + j, mode, d) +: rest, left)
            case Text(s)        => fitting(rest, left - s.size)
            case Break(sp, _) =>
              mode match {
                case Mode.Flat  => fitting(rest, left - sp)
                case Mode.Break => true
              }
            case Newline  => true
            case Group(d) => fitting((i, mode, d) +: rest, left)
          }
        else
          false
    }

  def startsWith(s: String): Boolean

}

private case object Empty extends Doc {
  def startsWith(s: String): Boolean = false
}

private case class Append(d1: Doc, d2: Doc) extends Doc {
  def startsWith(s: String): Boolean =
    if (d1 == Empty)
      d2.startsWith(s)
    else
      d1.startsWith(s)
}

private case class Nest(indent: Int, d: Doc) extends Doc {
  def startsWith(s: String): Boolean = d.startsWith(s)
}

private case class Text(text: String) extends Doc {
  def startsWith(s: String): Boolean = text.startsWith(s)
}

private case class Break(sp: Int, off: Int) extends Doc {
  def startsWith(s: String): Boolean = false
}

private case object Newline extends Doc {
  def startsWith(s: String): Boolean = false
}

private case class Group(d: Doc) extends Doc {
  def startsWith(s: String): Boolean = d.startsWith(s)
}

private sealed trait Mode
private object Mode {
  case object Flat extends Mode
  case object Break extends Mode
}
