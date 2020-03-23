package swam
package witx
package parser

import cats.effect.Effect
import fastparse._
import swam.witx.parser.parser.WitxWhitespace._
import swam.witx.unresolved.{BaseWitxType, _}

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
class ModuleParser[F[_]: Effect](val importContext: ImportContext[F],
                                 multipleResult: Boolean = false,
                                 expandArrayTypes: Boolean = true) {

  import swam.text.parser.Lexical._
  import TypesParser.tpe

  def file[_: P] =
    P(
      ws ~ deps
        .rep()
        .map(
          t => t.reduce((t1, t2) => t1 ++ t2)
        )
        .flatMap(types => module(types)) ~ ws ~ End)

  def module[_: P](types: Map[String, BaseWitxType]) =
    P(
      "(" ~ word("module") ~ id ~
        (`import`.map { case (name, tp) => ImportDeclaration(name, tp) }
          | interface(types))
          .rep(1) ~ ")").map {
      case (name, decs) => (types, ModuleInterface(name, decs))
    }

  def `import`[_: P] = {
    P("(" ~ word("import") ~ string ~ "(" ~ name ~ ")" ~ ")")
  }

  def interface[_: P](types: Map[String, BaseWitxType]): P[FunctionExport] = {
    P(
      "(" ~ word("@interface") ~ func(types) ~ ")"
    )
  }

  def splitResultsAndParamsByWASIType(params: Seq[Field], results: Seq[Field]): (Seq[Field], Seq[Field]) = {
    if (multipleResult)
      (params, results)
    else {
      if (results.length > 1) {
        return (params.concat(results.tail.map(t => Field(t.id, Pointer(t.tpe)))), Seq(results.head))
      }
      (params, results)
    }
  }

  def expandArrayTypes(field: Field, types: Map[String, BaseWitxType]): Seq[Field] =
    field.tpe match {
      case _: ArrayType => Seq(field, Field(s"${field.id}Len", types("u32")))
      case _            => Seq(field)
    }

  def func[_: P](types: Map[String, BaseWitxType]): P[FunctionExport] = {
    P(word("func") ~ "(" ~ word("export") ~ string ~ ")" ~ param(types).rep() ~ result(types).rep()).map {
      case (name, params, results) => {
        val paramsImports = splitResultsAndParamsByWASIType(params.flatten, results.flatten)
        FunctionExport(name, paramsImports._1, paramsImports._2)
      }
    }
  }

  def param[_: P](types: Map[String, BaseWitxType]): P[Seq[Field]] = {
    P("(" ~ word("param") ~ field(types) ~ ")")
  }

  def result[_: P](types: Map[String, BaseWitxType]): P[Seq[Field]] = {
    P("(" ~ word("result") ~ field(types) ~ ")")
  }

  def field[_: P](types: Map[String, BaseWitxType]): P[Seq[Field]] = {
    P(id ~ tpe(types)).map {
      case (name, tpe) => if (expandArrayTypes) expandArrayTypes(Field(name, tpe), types) else Seq(Field(name, tpe))
    }
  }

  def ptr[_: P](types: Map[String, BaseWitxType]): P[BaseWitxType] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ tpe(types) ~ ")")
  }

  def deps[_: P] = {
    P("(" ~ word("use") ~ string.map(t => importContext.load(t)) ~ ")")
  }

}

object ModuleParser {
  def apply[F[_]: Effect](importContext: ImportContext[F]): ModuleParser[F] =
    new ModuleParser[F](importContext)
}
