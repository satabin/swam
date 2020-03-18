package swam
package witx
package parser

import fastparse._
import swam.witx.parser.parser.WitxWhitespace._
import swam.witx.unresolved._

import scala.collection.MapView

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
object TypesParser {

  import Lexical._

  def file[_: P]: P[MapView[String, WitXType]] =
    P(ws ~ types ~ ws ~ End)
      .map(t => t.groupBy(i => i.name).view.mapValues(t => t.last))

  def types[_: P]: P[Seq[WitXType]] = {
    P("(" ~ word("typename") ~ id ~/ subtype ~ ")")
      .rep(1)
      .map(s => s.map { case (name, tpe) => new unresolved.WitXType(name, tpe) })
  }

  def subtype[_: P]: P[BaseWitxType] = {
    P(
      `type` | enum | flags | struct | array | handle | union
    ).map {
      case simpleType: String        => AliasType(simpleType)
      case complexType: BaseWitxType => complexType
    }
  }

  def struct[_: P]: P[StructType] = {
    P("(" ~ word("struct") ~ field.rep(1) ~ ")").map { fields =>
      StructType(fields)
    }
  }

  def array[_: P]: P[ArrayType] = {
    P("(" ~ word("array") ~ id ~ ")").map { i =>
      ArrayType(i)
    }
  }

  def pointer[_: P]: P[Pointer] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ `type` ~ ")").map { case tpe => Pointer(tpe) }
  }

  def handle[_: P]: P[Handle] = {
    P("(" ~ word("handle") ~ ")").map(_ => Handle())
  }

  def field[_: P]: P[Either[Field, PointerField]] = {
    P(
      "(" ~ word("field") ~ id ~ (id | pointer) ~ ")"
    ).map {
      case (name, tpe: String)  => Left(Field(name, tpe))
      case (name, tpe: Pointer) => Right(PointerField(name, tpe))
    }
  }

  def union[_: P]: P[UnionType] = {
    P(
      "(" ~ word("union") ~ id ~ field.rep(1) ~ ")"
    ).map { case (name, fields) => UnionType(name, fields) }
  }

  def enum[_: P]: P[EnumType] = {
    P("(" ~ word("enum") ~ `type` ~ id.rep(1) ~ ")").map {
      case (t, names) =>
        EnumType(t, names)
    }
  }

  def flags[_: P]: P[FlagsType] = {
    P("(" ~ word("flags") ~ `type` ~ id.rep(1) ~ ")").map { case (tpe, names) => FlagsType(tpe, names) }
  }

}
