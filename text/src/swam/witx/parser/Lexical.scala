package swam
package witx
package parser

import fastparse._
import NoWhitespace._
import swam.text.parser.Lexical.{idchar, linecomment}

/**
  * @author Javier Cabrera-Arteaga on 2020-03-18
  */
object Lexical {

  def comment[_: P]: P0 =
    P(linecomment)

  def ws[_: P]: P0 =
    P(NoTrace((CharIn(" \u0009\u000a\u000d") | comment).rep))

  @inline
  def word[_: P](s: String) =
    P(s ~ !idchar).opaque(s)

  def id[_: P]: P[String] =
    P("$" ~ idchar.rep(1).!)
}
