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
import NoWhitespace._

import java.lang.{Float => JFloat, Double => JDouble, Long => JLong}

object Lexical {

  def comment[_: P]: P0 =
    P(linecomment | blockcomment)

  def linecomment[_: P]: P0 =
    P(";;" ~/ CharsWhile(_ != '\u000a', 0))

  def blockcomment[_: P]: P0 =
    P("(;" ~/ (CharsWhile(!";(".contains(_)) | !";)" ~ ";" | !"(;" ~ "(" | blockcomment).rep ~ ";)")

  def ws[_: P]: P0 =
    P(NoTrace((CharIn(" \u0009\u000a\u000d") | comment).rep))

  def idchar[_: P]: P0 =
    P(CharIn("0-9A-Za-z!#$%&'*+\\-./:<=>?@\\\\^_`|~"))

  def keyword[_: P]: P0 =
    P(CharIn("a-z") ~ idchar.rep)

  def id[_: P]: P[String] =
    P("$" ~ idchar.rep(1).!)

  private def sign[_: P] =
    P(CharIn("+\\-").?.!.map {
      case "" | "+" => 1
      case "-"      => -1
    })

  private def digit[_: P] =
    P(CharIn("0-9").!.map(_(0) - '0'))

  private def hexdigit[_: P] =
    P(CharIn("0-9a-fA-F").!.map { s =>
      val c = s(0)
      if (c >= '0' && c <= '9')
        c - '0'
      else if (c >= 'a' && c <= 'f')
        c - 'a' + 10
      else
        c - 'A' + 10
    })

  private def num[_: P] =
    P(digit.rep(min = 1, sep = "_".?).map(_.foldLeft(BigInt(0))(10 * _ + _)))

  private def hexnum[_: P] =
    P(hexdigit.rep(min = 1, sep = "_".?).map(_.foldLeft(BigInt(0))(16 * _ + _)))

  def uint32[_: P] =
    P(
      "0x" ~ hexnum.!.map(_.replaceAll("_", "")).map(Integer.parseUnsignedInt(_, 16)) | num.!.map(_.replaceAll("_", ""))
        .map(Integer.parseUnsignedInt(_)))

  def int32[_: P] =
    P(
      sign.! ~ ("0x" ~ hexnum.!.map(_.replaceAll("_", "")).map(Integer.parseUnsignedInt(_, 16)) | num.!.map(
        _.replaceAll("_", "")).map(Integer.parseUnsignedInt(_)))).map {
      case (s, n) => if (s == "-") -1 * n else n
    }

  def int64[_: P] =
    P(
      sign.! ~ ("0x" ~ hexnum.!.map(_.replaceAll("_", "")).map(JLong.parseUnsignedLong(_, 16)) | num.!.map(
        _.replaceAll("_", "")).map(JLong.parseUnsignedLong(_)))).map {
      case (s, n) => if (s == "-") -1L * n else n
    }

  private def rfloat[_: P] =
    P(
      ("0x" ~ hexnum ~ "." ~ hexnum ~ CharIn("Pp") ~ sign ~ num).!
        | ("0x" ~ hexnum.! ~ (CharIn("Pp") ~ sign ~ num).!).map {
          case (s1, s2) => s"0x$s1.0$s2"
        }
        | ("0x" ~ hexnum.! ~ "." ~ (CharIn("Pp") ~ sign ~ num).!).map {
          case (s1, s2) => s"0x$s1.0$s2"
        }
        | ("0x" ~ hexnum ~ "." ~ hexnum.?).!.map(_ + "0p0")
        | ("0x" ~ hexnum).!.map(_ + ".0p0")
        | (num ~ ("." ~ num.?).? ~ (CharIn("Ee") ~ sign ~ num).?).!).map(_.replaceAll("_", ""))

  def float32[_: P] =
    P(
      (sign.! ~ rfloat).map { case (s, f) => if (s == "-") -1 * JFloat.parseFloat(f) else JFloat.parseFloat(f) }
        | (sign.! ~ "inf").map(s => if (s == "-") Float.NegativeInfinity else Float.PositiveInfinity)
        | (sign.! ~ "nan:0x" ~ hexnum).map {
          case (s, payload) =>
            if (s == "-") JFloat.intBitsToFloat(0xff800000 | payload.intValue)
            else JFloat.intBitsToFloat(0x7f800000 | payload.intValue)
        }
        | (sign.! ~ "nan").map(s =>
          if (s == "-") JFloat.intBitsToFloat(0xffc00000) else JFloat.intBitsToFloat(0x7fc00000)))

  def float64[_: P] =
    P((sign.! ~ rfloat).map { case (s, f) => if (s == "-") -1 * JDouble.parseDouble(f) else JDouble.parseDouble(f) }
      | (sign.! ~ "inf").map(s => if (s == "-") Double.NegativeInfinity else Double.PositiveInfinity)
      | (sign.! ~ "nan:0x" ~ hexnum).map {
        case (s, payload) =>
          if (s == "-") JDouble.longBitsToDouble(0XFFF0000000000000L | payload.longValue)
          else JDouble.longBitsToDouble(0X7FF0000000000000L | payload.longValue)
      }
      | (sign.! ~ "nan").map(s =>
        if (s == "-") JDouble.longBitsToDouble(0XFFF8000000000000L) else JDouble.longBitsToDouble(0X7FF8000000000000L)))

  def string[_: P] =
    P(
      "\"" ~ (CharsWhile(c => c >= '\u0020' && c != '\u007f' && c != '"' && c != '\\').!
        | P("\\t").map(_ => '\t')
        | P("\\n").map(_ => '\n')
        | P("\\r").map(_ => '\r')
        | P("\\\"").map(_ => '"')
        | P("\\'").map(_ => '\'')
        | P("\\\\").map(_ => '\\')
        | "\\" ~ (hexdigit ~ hexdigit).!.map(Integer.parseInt(_, 16).toByte)
        | "\\u" ~ hexnum.!.map(Integer.parseInt(_, 16))
          .filter(n => n < 0xd800 || (0xe000 <= n && n < 0x110000))
          .map(_.toChar)).rep.map(_.mkString) ~ "\""
    )

  def bstring[_: P] =
    P(
      "\"" ~ (CharPred(c => c >= '\u0020' && c != '\u007f' && c != '"' && c != '\\').!.map(_(0))
        | P("\\t").map(_ => '\t'.toChar)
        | P("\\n").map(_ => '\n'.toChar)
        | P("\\r").map(_ => '\r'.toChar)
        | P("\\\"").map(_ => '"'.toChar)
        | P("\\'").map(_ => '\''.toChar)
        | P("\\\\").map(_ => '\\'.toChar)
        | "\\" ~ (hexdigit ~ hexdigit).!.map(Integer.parseInt(_, 16).toChar)
        | "\\u" ~ hexnum.!.map(Integer.parseInt(_, 16))
          .filter(n => n < 0xd800 || (0xe000 <= n && n < 0x110000))
          .map(_.toChar)).map(Character.toChars(_).map(_.toByte)).rep.map(_.flatten.toArray) ~ "\""
    )

  @inline
  def word[_: P](s: String) =
    P(s ~ !idchar).opaque(s)

  def index[_: P] =
    P(uint32.map(Left(_)) | id.map(id => Right(SomeId(id))))
      .opaque("index or name")

}
