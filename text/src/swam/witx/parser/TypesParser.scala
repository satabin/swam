package swam
package witx
package parser

import java.nio.file.Path

import cats.effect._
import fastparse._
import swam.text.{ParserException, readFile}
import swam.witx.parser.parser.WitxWhitespace._
import swam.witx.unresolved._

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
object TypesParser {

  import text.parser.Lexical._

  private var declaredTypes = Map[String, BaseWitxType](
    "u32" -> BasicType("u32"),
    "u64" -> BasicType("u64"),
    "u8" -> BasicType("u8"),
    "u16" -> BasicType("u16"),
    "string" -> BasicType("string")
  )

  def file[_: P]: P[Map[String, BaseWitxType]] = {
    P(ws ~ types ~ ws ~ End).map(_ => declaredTypes)
  }

  def types[_: P] = {
    P("(" ~ word("typename") ~ id ~/ subtype ~ ")")
      .map {
        case (name, tp) => {
          declaredTypes = declaredTypes + (name -> tp)
        }
      }
      .rep(1)
  }

  def subtype[_: P]: P[BaseWitxType] = {
    P(
      name.map(t => AliasType(t)) | enum | flags | struct | array | handle | union
    )
  }

  def struct[_: P]: P[StructType] = {
    P("(" ~ word("struct") ~ field.rep(1) ~ ")").map { fields =>
      StructType(fields)
    }
  }

  def array[_: P]: P[ArrayType] = {
    P("(" ~ word("array") ~ id ~ ")").map { i =>
      ArrayType(declaredTypes(i))
    }
  }

  def pointer[_: P]: P[Pointer] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ tpe(declaredTypes) ~ ")").map {
      case tpe => Pointer(tpe)
    }
  }

  def handle[_: P]: P[Handle] = {
    P("(" ~ word("handle") ~ ")").map(_ => Handle())
  }

  def tpe[_: P](importCtx: Map[String, BaseWitxType]): P[BaseWitxType] = {
    P((id | name).map(x => {
      importCtx(x)
    }) | pointer)
  }

  def field[_: P]: P[Field] = {
    P(
      "(" ~ word("field") ~ id ~ tpe(declaredTypes) ~ ")"
    ).map {
      case (name, tpe) => Field(name, tpe)
    }
  }

  def union[_: P]: P[UnionType] = {
    P(
      "(" ~ word("union") ~ id ~ field.rep(1) ~ ")"
    ).map { case (name, fields) => UnionType(name, fields) }
  }

  def enum[_: P]: P[EnumType] = {
    P("(" ~ word("enum") ~ name ~ id.rep(1) ~ ")").map {
      case (t, names) =>
        EnumType(declaredTypes(t), names)
    }
  }

  def flags[_: P]: P[FlagsType] = {
    P("(" ~ word("flags") ~ name ~ id.rep(1) ~ ")").map { case (tpe, names) => FlagsType(declaredTypes(tpe), names) }
  }

}
