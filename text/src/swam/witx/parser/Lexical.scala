package swam
package witx
package parser

import fastparse._
import NoWhitespace._

/**
  * @author Javier Cabrera-Arteaga on 2020-03-18
  */
object Lexical {

  def linecomment[_: P]: P0 =
    P(";;" ~/ CharsWhile(_ != '\u000a', 0))

  def ws[_: P]: P0 =
    P(NoTrace((CharIn(" \u0009\u000a\u000d") | linecomment).rep))

  def idchar[_: P]: P0 =
    P(CharIn("0-9A-Za-z!#$%&'*+\\-./:<=>?@\\\\^_`|~"))

  @inline
  def word[_: P](s: String) =
    P(s ~ !idchar).opaque(s)

  def id[_: P]: P[String] =
    P("$" ~ idchar.rep(1).!)

  def name[_: P]: P[String] =
    P(idchar.rep(1).!)
}
