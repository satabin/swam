/*
 * Copyright 2018 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam
package text
package parser

import unresolved._

import fastparse._
import WastWhitespace._

object ModuleParsers {

  import Lexical._
  import Instructions._
  import Types._

  private def `type`[_: P]: P[Seq[Type]] =
    P((Index ~ word("type") ~/ id.? ~ functype ~ ")").map {
      case (idx, id, (p, tpe)) => Seq(Type(makeId(id), p, tpe)(idx))
    })

  private def importdesc[_: P]: P[ImportDesc] =
    P(
      ("(" ~ Index ~ ((word("func") ~/ id.?.map(makeId(_)) ~ typeuse)
        .map((ImportDesc.Func.apply _).tupled)
        | (word("table") ~/ id.?.map(makeId(_)) ~ tabletype)
          .map((ImportDesc.Table.apply _).tupled)
        | (word("memory") ~/ id.?.map(makeId(_)) ~ memtype)
          .map((ImportDesc.Memory.apply _).tupled)
        | (word("global") ~/ id.?.map(makeId(_)) ~ globaltype)
          .map((ImportDesc.Global.apply _).tupled))).map {
        case (idx, d) => d(idx)
      } ~ ")")

  private def `import`[_: P]: P[Seq[Import]] =
    P((Index ~ word("import") ~/ string ~ string ~ importdesc ~ ")").map {
      case (idx, mod, name, desc) => Seq(Import(mod, name, desc)(idx))
    })

  private def locals[_: P]: P[Seq[Local]] =
    P(
      ("(" ~ Index ~ word("local") ~/ ((id ~/ valtype).map {
        case (id, tpe) => Seq(Local(SomeId(id), tpe) _)
      }
        | valtype.rep.map(_.map(Local(NoId, _) _))) ~ ")").rep
        .map(_.flatMap {
          case (idx, fs) => fs.map(_(idx))
        }))

  private def inlineexport[_: P]: P[String] =
    P("(" ~ word("export") ~/ string ~ ")")

  private def function[_: P]: P[Seq[Field]] =
    P(
      (Index ~ word("func") ~/ id.? ~ inlineexport.rep ~ (NoCut(typeuse ~ locals ~ instrs)
        .map {
          case (tu, locals, instrs) =>
            (idx: Int, id: Option[String], exports: Seq[String]) => {
              val id1 =
                if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
              exports.map(Export(_, ExportDesc.Func(Right(id1))(idx))(idx)) :+ Function(id1, tu, locals, instrs)(idx)
            }
        }
        | ("(" ~ word("import") ~/ string ~ string ~ ")" ~ typeuse).map {
          case (name1, name2, tu) =>
            (idx: Int, id: Option[String], exports: Seq[String]) => {
              val id1 =
                if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
              exports
                .map(Export(_, ExportDesc.Func(Right(id1))(idx))(idx)) ++ Seq(
                Import(name1, name2, ImportDesc.Func(id1, tu)(idx))(idx))
            }
        }) ~ ")").map {
        case (idx, id, exports, f) => f(idx, id, exports)
      })

  private def table[_: P]: P[Seq[Field]] =
    P((Index ~ word("table") ~/ id.? ~ inlineexport.rep ~ (tabletype.map {
      case (tpe) =>
        (idx: Int, id: Option[String], exports: Seq[String]) => {
          val id1 = if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
          exports
            .map(Export(_, ExportDesc.Table(Right(id1))(idx))(idx)) :+ Table(id1, tpe)(idx)
        }
    }
      | (elemtype ~ "(" ~ word("elem") ~/ index.rep ~ ")").map {
        case (tpe, fs) =>
          (idx: Int, id: Option[String], exports: Seq[String]) => {
            val id1 = idOrFresh(id, idx)
            exports
              .map(Export(_, ExportDesc.Table(Right(id1))(idx))(idx)) ++ Seq(
              Table(id1, TableType(tpe, Limits(fs.size, Some(fs.size))))(idx),
              Elem(Right(id1), Seq(i32.Const(0)(idx)), fs)(idx)
            )
          }
      }
      | ("(" ~ word("import") ~/ string ~ string ~ ")" ~ tabletype).map {
        case (name1, name2, tpe) =>
          (idx: Int, id: Option[String], exports: Seq[String]) => {
            val id1 = if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
            exports.map(Export(_, ExportDesc.Table(Right(id1))(idx))(idx)) :+
              Import(name1, name2, ImportDesc.Table(id1, tpe)(idx))(idx)
          }
      }) ~ ")").map { case (idx, id, exports, f) => f(idx, id, exports) })

  private def memory[_: P]: P[Seq[Field]] =
    P((Index ~ word("memory") ~/ id.? ~ inlineexport.rep ~ (memtype.map {
      tpe => (idx: Int, id: Option[String], exports: Seq[String]) =>
        {
          val id1 = if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
          exports
            .map(Export(_, ExportDesc.Memory(Right(id1))(idx))(idx)) :+ Memory(id1, tpe)(idx)
        }
    }
      | ("(" ~ word("data") ~/ bstring.rep.map(_.flatten.toArray) ~ ")").map {
        ds => (idx: Int, id: Option[String], exports: Seq[String]) =>
          {
            val m = math.ceil(ds.length.toDouble / 65536).toInt
            val id1 = idOrFresh(id, idx)
            exports
              .map(Export(_, ExportDesc.Memory(Right(id1))(idx))(idx)) ++ Seq(Memory(id1, MemType(Limits(m, m)))(idx),
                                                                              Data(Right(id1),
                                                                                   Seq(i32.Const(0)(idx)),
                                                                                   ds)(idx))
          }
      }
      | ("(" ~ word("import") ~/ string ~ string ~ ")" ~ memtype).map {
        case (name1, name2, tpe) =>
          (idx: Int, id: Option[String], exports: Seq[String]) => {
            val id1 = if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
            val imp = Import(name1, name2, ImportDesc.Memory(id1, tpe)(idx))(idx)
            exports.map(Export(_, ExportDesc.Memory(Right(id1))(idx))(idx)) :+ imp
          }
      }) ~ ")").map { case (idx, id, exports, f) => f(idx, id, exports) })

  private def global[_: P]: P[Seq[Field]] =
    P((Index ~ word("global") ~/ id.? ~ inlineexport.rep ~ ((globaltype ~ expr)
      .map {
        case (tpe, e) =>
          (idx: Int, id: Option[String], exports: Seq[String]) => {
            val id1 = if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
            exports
              .map(Export(_, ExportDesc.Global(Right(id1))(idx))(idx)) :+ Global(id1, tpe, e)(idx)
          }
      }
      | ("(" ~ word("import") ~/ string ~ string ~ ")" ~ globaltype).map {
        case (name1, name2, tpe) =>
          (idx: Int, id: Option[String], exports: Seq[String]) =>
            val id1 = if (exports.isEmpty) makeId(id) else idOrFresh(id, idx)
            exports
              .map(Export(_, ExportDesc.Global(Right(id1))(idx))(idx)) :+ Import(name1,
                                                                                 name2,
                                                                                 ImportDesc.Global(id1, tpe)(idx))(idx)
      }) ~ ")").map { case (idx, id, exports, f) => f(idx, id, exports) })

  private def exportdesc[_: P]: P[ExportDesc] =
    P(
      "(" ~ (Index ~ ((word("func") ~/ index.map(ExportDesc.Func.apply))
        | (word("table") ~/ index.map(ExportDesc.Table.apply))
        | (word("memory") ~/ index.map(ExportDesc.Memory.apply))
        | (word("global") ~/ index.map(ExportDesc.Global.apply))))
        .map { case (idx, e) => e(idx) } ~ ")")

  private def export[_: P]: P[Seq[Export]] =
    P((Index ~ word("export") ~/ string ~ exportdesc ~ ")").map {
      case (idx, name, desc) => Seq(Export(name, desc)(idx))
    })

  private def start[_: P]: P[Seq[StartFunc]] =
    P((Index ~ word("start") ~/ index ~ ")").map {
      case (idx, f) => Seq(StartFunc(f)(idx))
    })

  private def elem[_: P]: P[Seq[Elem]] =
    P(
      (Index ~ word("elem") ~/ (index | Pass(Left(0))) ~ ("(" ~ word("offset") ~/ expr ~ ")" | foldedinstr | instr
        .map(Seq(_))) ~ index.rep ~ ")").map {
        case (idx, x, e, y) => Seq(Elem(x, e, y)(idx))
      })

  private def data[_: P]: P[Seq[Data]] =
    P(
      (Index ~ word("data") ~/ (index | Pass(Left(0))) ~ ("(" ~ word("offset") ~/ expr ~ ")" | foldedinstr) ~ bstring.rep
        .map(_.flatten.toArray) ~ ")").map {
        case (idx, x, e, b) => Seq(Data(x, e, b)(idx))
      })

  private def field[_: P]: P[Seq[Field]] =
    P("(" ~ (`type` | `import` | function | table | memory | global | export | start | elem | data))

  def fields[_: P]: P[Seq[Field]] =
    P(field.rep.map(_.flatten))

  def module[_: P]: P[Module] =
    P(
      (("(" ~ Index ~ word("module") ~/ id.? ~ fields ~ ")")
        | Index ~ Pass(None) ~ field.rep(1).map(_.flatten)).map {
        case (idx, id, flds) => Module(makeId(id), flds)(idx)
      })

  def file[_: P]: P[Module] =
    P(ws ~ module ~ ws ~ End)

  private def idOrFresh(id: Option[String], idx: Int): Id =
    id match {
      case Some(id) => SomeId(id)
      case None     => FreshId(idx)
    }

  private def makeId(id: Option[String]): Id =
    id match {
      case Some(id) => SomeId(id)
      case None     => NoId
    }

}
