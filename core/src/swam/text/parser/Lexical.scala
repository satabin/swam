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
package parser

import unresolved._

import fastparse._
import fastparse.all._

import java.lang.{Long => JLong}

import scala.annotation.tailrec

object Lexical {

  val comment: P0 =
    P(linecomment | blockcomment)

  val linecomment: P0 =
    P(";;" ~/ CharsWhile(_ != '\u000a', min = 0))

  val blockcomment: P0 =
    P("(;" ~/ (CharsWhile(!";(".contains(_)) | !";)" ~ ";" | !"(;" ~ "(" | blockcomment).rep ~ ";)")

  val ws: P0 =
    P(NoTrace((CharIn(" \u0009\u000a\u000d") | comment).rep))

  val white = WhitespaceApi.Wrapper {
    ws
  }

  val idchar: P0 =
    P(CharIn('0' to '9', 'A' to 'Z', 'a' to 'z', "!#$%&'*+-./:<=>?@\\^_`|~"))

  val keyword: P0 =
    P(CharIn('a' to 'z') ~ idchar.rep)

  val id: P0 =
    P("$" ~ idchar.rep(min = 1))

  private val sign: P[Int] =
    CharIn("+-").?.!.map {
      case "" | "+" => 1
      case "-"      => -1
    }

  private val digit: P[Int] =
    CharIn('0' to '9').!.map(_(0) - '0')

  private val hexdigit: P[Int] =
    CharIn('0' to '9', 'A' to 'F', 'a' to 'f').!.map { s =>
      val c = s(0)
      if (c >= '0' && c <= '9')
        c - '0'
      else if (c >= 'a' && c <= 'f')
        c - 'a' + 10
      else
        c - 'A' + 10
    }

  private val num: P[BigInt] =
    P(digit.rep(min = 1, sep = "_".?).map(_.foldLeft(BigInt(0))(10 * _ + _)))

  private val hexnum: P[BigInt] =
    P(hexdigit.rep(min = 1, sep = "_".?).map(_.foldLeft(BigInt(0))(16 * _ + _)))

  val unsigned: P[BigInt] =
    P(("0x" ~ hexnum | num))

  val signed: P[BigInt] =
    P((sign ~ (("0x" ~ hexnum) | num))).map { case (sign, n) => sign * n }

  val uint32: P[Int] =
    P(unsigned.map(_.intValue))

  val int32: P[Int] =
    P(signed.map(_.intValue))

  val int64: P[Long] =
    P(signed.map(_.longValue))

  private val rfloat: P[BigDecimal] =
    P(num ~ ("." ~ num.?).? ~ (CharIn("Ee") ~ sign ~ num).?).map {
      case (p, q, e) =>
        val pd = BigDecimal(p)
        val qd =
          q.map(q => BigDecimal(q.getOrElse(BigInt(0)), countdigits(q.getOrElse(BigInt(0)), 0)))
            .getOrElse(BigDecimal(0))
        val (sign, ed) = e
          .map { case (sign, num) => (sign, BigDecimal(num)) }
          .getOrElse((1, BigDecimal(0)))
        if (sign > 0)
          (pd + qd) * BigDecimal(BigInt(10).pow(ed.intValue))
        else
          (pd + qd) / BigDecimal(BigInt(10).pow(ed.intValue))
    }

  private val rhexfloat: P[BigDecimal] =
    P("0x" ~ hexnum ~ ("." ~ hexnum.?).? ~ (CharIn("Pp") ~ sign ~ num).?).map {
      case (p, q, e) =>
        val pd = BigDecimal(p)
        val qd =
          q.map(q => BigDecimal(q.getOrElse(BigInt(0)), countdigits(q.getOrElse(BigInt(0)), 0)))
            .getOrElse(BigDecimal(0))
        val (sign, ed) = e
          .map { case (sign, num) => (sign, BigDecimal(num)) }
          .getOrElse((1, BigDecimal(0)))
        if (sign > 0)
          (pd + qd) * BigDecimal(BigInt(2).pow(ed.intValue))
        else
          (pd + qd) / BigDecimal(BigInt(2).pow(ed.intValue))
    }

  @tailrec
  private def countdigits(bi: BigInt, acc: Int): Int =
    if (bi == BigInt(0))
      acc
    else
      countdigits(bi / 10, acc + 1)

  private val float: P[F] =
    P(
      sign.flatMap(
        sign =>
          rhexfloat.map(d => F.Value(sign, d))
            | rfloat.map(d => F.Value(sign, d))
            | P("inf") ~ PassWith(if (sign < 0) F.MInf else F.PInf)
            | ("nan:0x" ~ hexnum).map(_ => F.NaN)
            | P("nan") ~ PassWith(F.NaN)
      )
    )

  val float32: P[Float] =
    float.map {
      case F.Value(s, v) => s * v.floatValue
      case F.MInf     => Float.NegativeInfinity
      case F.PInf     => Float.PositiveInfinity
      case F.NaN      => Float.NaN
    }

  val float64: P[Double] =
    float.map {
      case F.Value(s, v) => s * v.doubleValue
      case F.MInf     => Double.NegativeInfinity
      case F.PInf     => Double.PositiveInfinity
      case F.NaN      => Double.NaN
    }

  val string: P[String] =
    P(
      "\"" ~ (CharsWhile(c => c >= '\u0020' && c != '\u007f' && c != '"' && c != '\\').!
        | P("\\t").map(_ => '\t')
        | P("\\n").map(_ => '\n')
        | P("\\r").map(_ => '\r')
        | P("\\\"").map(_ => '"')
        | P("\\'").map(_ => '\'')
        | P("\\\\").map(_ => '\\')
        | "\\" ~ (hexdigit ~ hexdigit).!.map(Integer.parseInt(_, 16).toChar)
        | "\\u" ~ hexnum.!.map(Integer.parseInt(_, 16))
          .filter(n => n < 0xd800 || (0xe000 <= n && n < 0x110000))
          .map(_.toChar)).rep.map(_.mkString) ~ "\""
    )

  def word(s: String): P0 =
    P(s ~ !idchar).opaque(s)

  val index: P[Index] =
    P(uint32.map(Left(_)) | id.!.map(id => Right(SomeId(id))))
      .opaque("index or name")

}

private sealed trait F
private object F {
  case class Value(sign: Int, bd: BigDecimal) extends F
  case object MInf extends F
  case object PInf extends F
  case object NaN extends F
}
