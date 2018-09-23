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

package swam.util

import scala.annotation.tailrec

/** Implementation of a pretty printer inspired by Wadler's one
  * and adapted to strict language (port of [[https://github.com/kfl/wpp]]).
  */
package object pretty {

  val empty: Doc =
    Empty

  def nest(indent: Int, d: Doc): Doc =
    Nest(indent, d)

  def str(t: String): Doc =
    Text(t)

  def break(sp: Int, off: Int): Doc =
    Break(sp, off)

  val line: Doc =
    Break(1, 0)

  val space: Doc =
    Text(" ")

  val newline: Doc =
    Newline

  def group(d: Doc): Doc =
    Group(d)

  def concat(ds: Seq[Doc]): Doc =
    ds.foldRight(empty)(_ ++ _)

  def seq[T](sep: Doc, ts: Seq[T])(implicit T: Pretty[T]): Doc =
    seq(sep, T.pretty _, ts)

  def seq(sep: Doc, ts: Seq[Doc]): Doc =
    seq(sep, identity[Doc] _, ts)

  def seq[T](sep: Doc, render: T => Doc, ts: Seq[T]): Doc = {
    @tailrec
    def iter(ts: Seq[T], acc: Doc): Doc =
      ts match {
        case Seq()             => acc
        case Seq(t)            => acc ++ render(t)
        case Seq(t, rest @ _*) => iter(rest, (acc ++ render(t) ++ sep))
      }
    iter(ts, empty)
  }

  def fromConv[T](conv: T => String, t: T): Doc =
    Text(conv(t))

  def int(i: Int): Doc =
    fromConv[Int](_.toString, i)

  implicit object IntPretty extends Pretty[Int] {
    def pretty(i: Int): Doc = int(i)
  }

  implicit object DocPretty extends Pretty[Doc] {
    def pretty(d: Doc): Doc = d
  }

  implicit class PrettyOps[T](val t: T) extends AnyVal {
    def pretty(implicit T: Pretty[T]): Doc =
      T.pretty(t)
  }

}
