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

object Types {

  import Lexical._

  def valtype[_: P]: P[ValType] =
    P(
      word("i32").map(_ => ValType.I32)
        | word("i64").map(_ => ValType.I64)
        | word("f32").map(_ => ValType.F32)
        | word("f64").map(_ => ValType.F64)
    )

  def resulttype[_: P]: P[ResultType] =
    P(result.rep.map(tpes => ResultType(tpes.toVector)))

  def blocktype[_: P]: P[TypeUse] =
    typeuse

  def functype[_: P]: P[(Vector[Id], FuncType)] =
    P(("(" ~ word("func") ~ params ~ results ~ ")").map {
      case (params, results) =>
        val (ns, ps) = params.unzip
        (ns, FuncType(ps, results))
    })

  private def params[_: P]: P[Vector[Param]] =
    P(
      (
        ("(" ~ word("param") ~ id.map(SomeId(_)) ~ valtype ~ ")").rep(1)
          | ("(" ~ word("param") ~ valtype.rep
            .map(_.map(NoId -> _)) ~ ")")
      ).rep.map(_.flatten.toVector))

  private def results[_: P]: P[Vector[ValType]] =
    P(
      ("(" ~ word("result") ~/ valtype.rep ~ ")").rep
        .map(_.flatten.toVector)
    )

  private def result[_: P]: P[ValType] =
    P("(" ~ word("result") ~/ valtype ~ ")")

  def limits[_: P]: P[Limits] =
    P((uint32 ~ uint32.?).map { case (min, max) => new Limits(min, max) })

  def memtype[_: P]: P[MemType] =
    P(limits.map(MemType))

  def tabletype[_: P]: P[TableType] =
    P((limits ~ elemtype).map { case (l, t) => TableType(t, l) })

  def elemtype[_: P]: P[ElemType] =
    P(word("funcref").map(_ => ElemType.FuncRef))

  def globaltype[_: P]: P[GlobalType] =
    P(
      valtype.map(GlobalType(_, Mut.Const))
        | "(" ~ word("mut") ~/ valtype.map(GlobalType(_, Mut.Var)) ~ ")"
    )

  def typeuse[_: P]: P[TypeUse] =
    P(
      (("(" ~ word("type") ~ index ~ ")").? ~ params ~ results)
        .map(TypeUse.tupled))
}
