package swam.witx.parser

import fastparse._
import swam.binary.ModuleParser
import swam.witx.parser.parser.WitxWhitespace._
import swam.witx.unresolved._
import swam.text.parser.Lexical.string

import scala.collection.MapView

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
class ModuleParser(val importContext: ImportContext) {

  import Lexical._
  import TypesParser.tpe

  def file[_: P] =
    P(ws ~ deps.rep() ~ module ~ ws ~ End).map { case (deps, mod) => mod }

  def module[_: P] =
    P(
      "(" ~ word("module") ~ id ~
        (`import`.map { case (name, tp) => ImportDeclaration(name, tp) }
          | interface)
          .rep(1) ~ ")").map {
      case (name, decs) => ModuleInterface(name, decs)
    }

  def `import`[_: P] = {
    P("(" ~ word("import") ~ string ~ "(" ~ name ~ ")" ~ ")")
  }

  def interface[_: P]: P[FunctionExport] = {
    P(
      "(" ~ word("@interface") ~ func ~ ")"
    )
  }

  def func[_: P]: P[FunctionExport] = {
    P(word("func") ~ "(" ~ word("export") ~ string ~ ")" ~ param.rep() ~ result.rep()).map {
      case (name, params, results) => FunctionExport(name, params, results)
    }
  }

  def param[_: P]: P[Field] = {
    P("(" ~ word("param") ~ id ~ tpe(importContext.types) ~ ")").map { case (name, tpe) => Field(name, tpe) }
  }

  def result[_: P]: P[Field] = {
    P("(" ~ word("result") ~ id ~ tpe(importContext.types) ~ ")").map { case (name, tpe) => Field(name, tpe) }
  }

  def ptr[_: P]: P[BaseWitxType] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ tpe(importContext.types) ~ ")")
  }

  def deps[_: P] = {
    P("(" ~ word("use") ~ string ~ ")")
  }

}

object ModuleParser {
  def apply(importContext: ImportContext): ModuleParser = new ModuleParser(importContext)
}
