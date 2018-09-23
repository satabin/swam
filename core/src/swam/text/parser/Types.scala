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

import fastparse.noApi._

object Types {

  import Lexical._
  import white._

  val valtype: P[ValType] =
    P(
      word("i32").map(_ => ValType.I32)
        | word("i64").map(_ => ValType.I64)
        | word("f32").map(_ => ValType.F32)
        | word("f64").map(_ => ValType.F64)
    )

  val resulttype: P[ResultType] =
    P(result.?.map(ResultType(_)))

  val functype: P[(Vector[Id], FuncType)] =
    P("(" ~ word("func") ~ params ~ results ~ ")").map {
      case (params, results) =>
        val (ns, ps) = params.unzip
        (ns, FuncType(ps, results))
    }

  private val params: P[Vector[Param]] =
    P(
      ("(" ~ word("param") ~ id.map(SomeId(_)) ~ valtype ~ ")").rep(min = 1)
        | ("(" ~ word("param") ~ valtype.rep
          .map(_.map(NoId -> _)) ~ ")")
    ).rep.map(_.flatten.toVector)

  private val results: P[Vector[ValType]] =
    P(
      ("(" ~ word("result") ~/ valtype.rep ~ ")").rep
        .map(_.flatten.toVector)
    )

  private val result: P[ValType] =
    P("(" ~ word("result") ~/ valtype ~ ")")

  val limits: P[Limits] =
    P(uint32 ~ uint32.?).map { case (min, max) => new Limits(min, max) }

  val memtype: P[MemType] =
    limits.map(MemType)

  val tabletype: P[TableType] =
    P(limits ~ elemtype).map { case (l, t) => TableType(t, l) }

  val elemtype: P[ElemType] =
    P(word("anyfunc").map(_ => ElemType.AnyFunc))

  val globaltype: P[GlobalType] =
    P(
      valtype.map(GlobalType(_, Mut.Const))
        | "(" ~ word("mut") ~/ valtype.map(GlobalType(_, Mut.Var)) ~ ")"
    )

  def typeuse: P[TypeUse] =
    P(("(" ~ word("type") ~ index ~ ")").? ~ params ~ results)
      .map(TypeUse.tupled)
}
