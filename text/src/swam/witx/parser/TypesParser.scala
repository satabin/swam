package swam
package witx
package parser

import fastparse._
import swam.witx.parser.parser.WitxWhitespace._
import swam.witx.unresolved._

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
object TypesParser {

  import Lexical._

  def file[_: P]: P[Boolean] =
    P(ws ~ types ~ ws ~ End).map(_ => true)

  def types[_: P]: P[Boolean] = {
    P(
      ("(" ~ word("typename") ~ id ~ subtype ~ ")")
        .rep(1)
        .log
        .map(_ => true))
  }

  def subtype[_: P]: P[Boolean] = {
    P(
      `type` | enum | flags | struct | array | handle | union
    ).map(_ => true)
  }

  def struct[_: P]: P[Boolean] = {
    P("(" ~ word("struct") ~ field.rep(1) ~ ")").log.map(_ => true)
  }

  def array[_: P]: P[Boolean] = {
    P("(" ~ word("array") ~ id ~ ")").map(_ => true)
  }

  def pointer[_: P]: P[Boolean] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ `type` ~ ")").map(_ => true)
  }

  def handle[_: P]: P[Boolean] = {
    P("(" ~ word("handle") ~ ")").map(_ => true)
  }

  def field[_: P]: P[Boolean] = {
    P(
      "(" ~ word("field") ~ id ~ (id | pointer) ~ ")"
    ).map(_ => true)
  }

  def union[_: P]: P[Boolean] = {
    P(
      "(" ~ word("union") ~ id ~ field.rep(1) ~ ")"
    ).map(_ => true)
  }

  def enum[_: P]: P[EnumType] = {
    P("(" ~ word("enum") ~ `type` ~ id.rep(1) ~ ")").map { _ =>
      EnumType(1)
    } // (enum u8
  }

  def flags[_: P]: P[Boolean] = {
    P("(" ~ word("flags") ~ `type` ~ id.rep(1) ~ ")").map(_ => true)
  }

}
