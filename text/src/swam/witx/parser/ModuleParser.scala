package swam.witx.parser

import fastparse._
import swam.witx.parser.parser.WitxWhitespace._
import swam.witx.unresolved._
import swam.text.parser.Lexical.string
import scala.collection.MapView

/**
    @author Javier Cabrera-Arteaga on 2020-03-18
  */
object ModuleParser {

  import Lexical._

  def file[_: P]: P[Boolean] =
    P(ws ~ deps ~ module ~ ws ~ End)
      .map(t => true)

  def module[_: P]: P[Boolean] =
    P("(" ~ word("module") ~ id ~ (`import` | interface).rep(1) ~ ")")
      .map(t => true)

  def `import`[_: P] = {
    P("(" ~ word("import") ~ string ~ "(" ~ name ~ ")" ~ ")")
  }

  def interface[_: P] = {
    P(
      "(" ~ word("@interface") ~ func ~ ")"
    ).log
  }

  def func[_: P] = {
    P(word("func") ~ "(" ~ word("export") ~ string ~ ")" ~ params.rep() ~ result.rep()).log
  }

  def params[_: P] = {
    P("(" ~ word("param") ~ id ~ tpe ~ ")").log
  }

  def result[_: P] = {
    P("(" ~ word("result") ~ id ~ tpe ~ ")").log
  }

  def tpe[_: P]: P[Boolean] = {
    P(id | name | ptr).log.map { _ =>
      true
    }
  }

  def ptr[_: P] = {
    P("(" ~ word("@witx") ~ (word("pointer") | word("const_pointer")) ~ tpe ~ ")").log
  }

  def deps[_: P] = {
    P("(" ~ word("use") ~ string ~ ")")
  }
}
