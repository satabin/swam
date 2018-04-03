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
package validation

import syntax._

import cats._
import cats.implicits._

import scala.language.higherKinds

abstract class Validator[F[_]](implicit val F: MonadError[F, Throwable]) {

  type Ctx = Context[F]

  protected val Ok = F.unit

  def validateAll[T](elements: Vector[T], validation: T => F[Unit]): F[Unit] =
    F.tailRecM(0) { idx =>
      if (idx < elements.size)
        validation(elements(idx)).map(_ => Left(idx + 1))
      else
        F.pure(Right(()))
    }

  def validateAll[T](elements: Vector[T], ctx: Ctx, validation: (T, Ctx) => F[Unit]): F[Unit] =
    F.tailRecM(0) { idx =>
      if (idx < elements.size)
        validation(elements(idx), ctx).map(_ => Left(idx + 1))
      else
        F.pure(Right(()))
    }

  def validateValType(tpe: ValType): F[Unit]

  def validateResultType(tpe: ResultType): F[Unit]

  def validateLimits(limits: Limits): F[Unit]

  def validateMemType(tpe: MemType): F[Unit]

  def validateFuncType(tpe: FuncType): F[Unit]

  def validateTableType(tpe: TableType): F[Unit]

  def validateElemType(tpe: ElemType): F[Unit]

  def validateGlobalType(tpe: GlobalType): F[Unit]

  def validateExternType(tpe: ExternType): F[Unit]

  def validate(inst: Inst, ctx: Ctx): F[Ctx]

  def validateFunction(tpe: Func, ctx: Ctx): F[Unit]

  def validateTable(tpe: TableType, ctx: Ctx): F[Unit]

  def validateMem(tpe: MemType, ctx: Ctx): F[Unit]

  def validateGlobal(global: Global, ctx: Ctx): F[Unit]

  def validateElem(elem: Elem, ctx: Ctx): F[Unit]

  def validateData(data: Data, ctx: Ctx): F[Unit]

  def validateStart(start: FuncIdx, ctx: Ctx): F[Unit]

  def validateImport(imp: Import, ctx: Ctx): F[Unit]

  def validateExport(exp: Export, ctx: Ctx): F[Unit]

  def validateConst(intr: Vector[Inst], ctx: Ctx): F[Unit]

  def validateConst(intr: Inst, ctx: Ctx): F[Unit]

}
