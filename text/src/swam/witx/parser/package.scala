package swam.witx.parser

import fastparse._

import swam.text.parser.Lexical

package object parser {

  object WitxWhitespace {
    implicit val whitespace = { implicit ctx: ParsingRun[_] =>
      Lexical.ws
    }
  }

}
