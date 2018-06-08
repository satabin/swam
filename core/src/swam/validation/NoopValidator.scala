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

import scala.language.higherKinds

class NoopValidator[F[_]](implicit F: MonadError[F, Throwable]) extends Validator[F] {
  val ok = F.pure(())
  def validateConst(intr: Inst, ctx: Ctx): F[Unit] = ok
  def validateConst(intr: Vector[Inst], ctx: Ctx): F[Unit] = ok
  def validate(inst: Inst, ctx: Ctx): F[Ctx] = F.pure(ctx)
  def validateData(data: Data, ctx: Ctx): F[Unit] = ok
  def validateElem(elem: Elem, ctx: Ctx): F[Unit] = ok
  def validateExport(exp: Export, ctx: Ctx): F[Unit] = ok
  def validateFunction(tpe: Func, ctx: Ctx): F[Unit] = ok
  def validateGlobal(global: Global, ctx: Ctx): F[Unit] = ok
  def validateImport(imp: Import, ctx: Ctx): F[Unit] = ok
  def validateMem(tpe: MemType, ctx: Ctx): F[Unit] = ok
  def validateStart(start: FuncIdx, ctx: Ctx): F[Unit] = ok
  def validateTable(tpe: TableType, ctx: Ctx): F[Unit] = ok
  def validateElemType(tpe: ElemType): F[Unit] = ok
  def validateExternType(tpe: ExternType): F[Unit] = ok
  def validateFuncType(tpe: FuncType): F[Unit] = ok
  def validateGlobalType(tpe: GlobalType): F[Unit] = ok
  def validateLimits(limits: Limits): F[Unit] = ok
  def validateMemType(tpe: MemType): F[Unit] = ok
  def validateResultType(tpe: ResultType): F[Unit] = ok
  def validateTableType(tpe: TableType): F[Unit] = ok
  def validateValType(tpe: ValType): F[Unit] = ok
  override def validateAll[T](elements: Vector[T], validation: T => F[Unit]): F[Unit] = ok
  override def validateAll[T](elements: Vector[T], ctx: Ctx, validation: (T, Ctx) => F[Unit]): F[Unit] = ok
}
