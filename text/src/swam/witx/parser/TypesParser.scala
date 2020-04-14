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

  // TODO change
  private var declaredTypes = Map[String, BaseWitxType](
    "u32" -> BasicType("u32", 4),
    "u64" -> BasicType("u64", 8),
    "s64" -> BasicType("s64", 8),
    "u8" -> BasicType("u8", 1),
    "u16" -> BasicType("u16", 2),
    "string" -> BasicType("string", 4),
    "ptr" -> BasicType("ptr", 4)
  )

  def file[_: P]: P[Map[String, BaseWitxType]] = {
    P(ws ~ types ~ ws ~ End).map(_ => declaredTypes)
  }

  def types[_: P] = {
    P(
      "(" ~ word("typename")
        ~ id.flatMap(id => subtype(id)) ~ ")")
      .map {
        case (tp) => {
          declaredTypes = declaredTypes + (tp.tpeName -> tp)
        }
      }
      .rep(1)
  }

  def subtype[_: P](typeId: String): P[BaseWitxType] = {
    P(
      name.map(t => {
        if (!declaredTypes.contains(t)) throw new Exception(s"Type $t not found")
        AliasType(typeId, declaredTypes(t))
      }) | enum(typeId) | flags(typeId) | struct(typeId) | array(typeId) | handle(typeId) | union(typeId)
    )
  }

  def struct[_: P](typeId: String): P[StructType] = {
    P("(" ~ word("struct") ~ field.rep(1) ~ ")").map { fields =>
      StructType(typeId, fields)
    }
  }

  def array[_: P](typeId: String): P[ArrayType] = {
    P("(" ~ word("array") ~ id ~ ")").map { i =>
      ArrayType(typeId, declaredTypes(i))
    }
  }

  def pointer[_: P]: P[Pointer] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ tpe(declaredTypes) ~ ")").map {
      case tpe => Pointer(tpe)
    }
  }

  def handle[_: P](typeId: String): P[Handle] = {
    P("(" ~ word("handle") ~ ")").map(_ => Handle(typeId))
  }

  def tpe[_: P](importCtx: Map[String, BaseWitxType]): P[BaseWitxType] = {
    P((id | name).map(importCtx) | pointer)
  }

  def field[_: P]: P[Field] = {
    P(
      "(" ~ word("field") ~ id ~ tpe(declaredTypes) ~ ")"
    ).map {
      case (name, tpe) => Field(name, tpe, isResult = false)
    }
  }

  def union[_: P](typeId: String): P[UnionType] = {
    P(
      "(" ~ word("union") ~ id ~ field.rep(1) ~ ")"
    ).map { case (name, fields) => UnionType(typeId, name, fields) }
  }

  def enum[_: P](typeId: String): P[EnumType] = {
    P("(" ~ word("enum") ~ name ~ id.rep(1) ~ ")").map {
      case (t, names) =>
        EnumType(typeId, declaredTypes(t), names)
    }
  }

  def flags[_: P](typeId: String): P[FlagsType] = {
    P("(" ~ word("flags") ~ name ~ id.rep(1) ~ ")").map {
      case (tpe, names) => FlagsType(typeId, declaredTypes(tpe), names)
    }
  }

}
