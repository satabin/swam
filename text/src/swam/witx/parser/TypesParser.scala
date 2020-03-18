package swam
package witx
package parser

import fastparse._
import swam.text.parser.{Instructions, Lexical, Types}
import swam.witx.parser.parser.WitxWhitespace._

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
object TypesParser {

  import Lexical._

  def types[_: P]: P[Boolean] = {
    P(
      ("(" ~ word("typename") ~ "$" ~ id ~ subtype ~ ")")
        .map(_ => true))
  }

  def subtype[_: P]: P[Boolean] = {
    P(
      id | // Simple number
        enum |
        flags |
        struct
    ).map(_ => true)
  }

  def struct[_: P]: P[Boolean] = {
    P("(" ~ word("struct") ~ field.rep(1) ~ ")").map(_ => true)
  }

  def field[_: P]: P[Boolean] = {
    P(
      "(" ~ word("field") ~ "$" ~ id ~ "$" ~ id ~ ")"
    ).map(_ => true)
  }

  def enum[_: P]: P[Boolean] = {
    P("(" ~ word("enum") ~ id ~ ("$" ~ id).rep(1) ~ ")").map(_ => true) // (enum u8
  }

  def flags[_: P]: P[Boolean] = {
    P("(" ~ word("flags") ~ id ~ ("$" ~ id).rep(1) ~ ")").map(_ => true)
  }

}
