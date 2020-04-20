package swam
package witx
package parser

import java.nio.file.Path

import cats.effect._
import fastparse._
import swam.text.{ParserException, readFile}
import swam.witx.parser.parser.WitxWhitespace._
import swam.witx.unresolved._

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
object TypesParser {

  import text.parser.Lexical._

  private val declaredTypes = Map[String, BaseWitxType](
    "u32" -> BasicType("u32", 4),
    "u64" -> BasicType("u64", 8),
    "s64" -> BasicType("s64", 8),
    "u8" -> BasicType("u8", 1),
    "u16" -> BasicType("u16", 2),
    "string" -> BasicType("string", 4),
    "ptr" -> BasicType("ptr", 4)
  )

  def file[_: P]: P[Map[String, BaseWitxType]] = {
    P(ws ~ _types(declaredTypes) ~ ws ~ End)
  }

  def _types[_: P](types: Map[String, BaseWitxType]): P[Map[String, BaseWitxType]] = {
    P(
      _type(types).flatMap(t => _types(t)) |
        _type(types)
    )
  }

  def _type[_: P](types: Map[String, BaseWitxType]): P[Map[String, BaseWitxType]] = {
    P("(" ~ word("typename") ~ id.flatMap(id => subtype(id, types)) ~ ")")
      .map(t => HashMap[String, BaseWitxType](t.tpeName -> t) ++ types)
  }

  def subtype[_: P](typeId: String, types: Map[String, BaseWitxType]): P[BaseWitxType] = {
    P(
      name
        .map(
          t => {
            if (!types.contains(t)) throw new Exception(s"Type $t not found")
            AliasType(typeId, types(t))
          }) | enum(typeId, types) | flags(typeId, types) | struct(typeId, types) | array(typeId, types) | handle(
        typeId) | union(typeId, types)
    )
  }

  def struct[_: P](typeId: String, types: Map[String, BaseWitxType]): P[StructType] = {
    P("(" ~ word("struct") ~ field(types).rep(1) ~ ")").map { fields =>
      StructType(typeId, fields)
    }
  }

  def array[_: P](typeId: String, types: Map[String, BaseWitxType]): P[ArrayType] = {
    P("(" ~ word("array") ~ id ~ ")").map { i =>
      ArrayType(typeId, types(i))
    }
  }

  def pointer[_: P](types: Map[String, BaseWitxType]): P[Pointer] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ tpe(types) ~ ")").map {
      case tpe => Pointer(tpe)
    }
  }

  def handle[_: P](typeId: String): P[Handle] = {
    P("(" ~ word("handle") ~ ")").map(_ => Handle(typeId))
  }

  def tpe[_: P](importCtx: Map[String, BaseWitxType]): P[BaseWitxType] = {
    P((id | name).map(importCtx) | pointer(importCtx))
  }

  def field[_: P](importCtx: Map[String, BaseWitxType]): P[Field] = {
    P(
      "(" ~ word("field") ~ id ~ tpe(importCtx) ~ ")"
    ).map {
      case (name, tpe) => Field(name, tpe, isResult = false)
    }
  }

  def union[_: P](typeId: String, importCtx: Map[String, BaseWitxType]): P[UnionType] = {
    P(
      "(" ~ word("union") ~ id ~ field(importCtx).rep(1) ~ ")"
    ).map { case (name, fields) => UnionType(typeId, name, fields) }
  }

  def enum[_: P](typeId: String, importCtx: Map[String, BaseWitxType]): P[EnumType] = {
    P("(" ~ word("enum") ~ name ~ id.rep(1) ~ ")").map {
      case (t, names) =>
        EnumType(typeId, importCtx(t), names)
    }
  }

  def flags[_: P](typeId: String, importCtx: Map[String, BaseWitxType]): P[FlagsType] = {
    P("(" ~ word("flags") ~ name ~ id.rep(1) ~ ")").map {
      case (tpe, names) => FlagsType(typeId, importCtx(tpe), names)
    }
  }

}
