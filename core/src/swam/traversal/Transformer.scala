/*
 * Copyright 2019 Lucas Satabin
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
package traversal

import syntax._

import cats._
import cats.implicits._

/** Transforms a WebAssembly program into another one.
  * Tree traversal is performed in bottom-up order, with a _preparation_ phase
  * for recursive instructions to upgrade the context before inspecting children.
  *
  * Every instruction has a default noop `transform` method, as well as a instruction categories
  * (see group `Category` for [[swam.syntax.Inst instructions]]).
  *
  * To implement your own transformation, you can override the methods that need specific handling.
  *
  * A [[Transformer]] is a [[cats.Semigroup]], two transformers can be merged into one, based on the
  * approach presented in [[https://infoscience.epfl.ch/record/228518/ Miniphases: Compilation using Modular and Efficient Tree Transformations]].
  * When merged, two transformers are fused, making it possible to perform both transformations in one
  * tree traversal only. Make sure your transformations respect the criteria defined in section 6 of the Miniphase paper
  * to fuse them, otherwise, behavior of the result won't be the expected one.
  * If not, transformations cannot be fused, and must be chained one after the other and perform
  * two tree traversals.
  */
class Transformer[F[_], Ctx](implicit F: Monad[F]) {

  protected[this] val id: (Ctx, Inst) => F[(Ctx, Inst)] = (ctx: Ctx, i: Inst) => F.pure(ctx -> i)
  protected[this] val fst: (Ctx, Inst) => F[Ctx] = (ctx: Ctx, i: Inst) => F.pure(ctx)

  protected[this] val constTransformId: (Ctx, AConst) => F[(Ctx, Inst)] = {
    case (ctx, c @ i32.Const(_)) => i32ConstTransform(ctx, c)
    case (ctx, c @ i64.Const(_)) => i64ConstTransform(ctx, c)
    case (ctx, c @ f32.Const(_)) => f32ConstTransform(ctx, c)
    case (ctx, c @ f64.Const(_)) => f64ConstTransform(ctx, c)
  }
  val constTransform: (Ctx, AConst) => F[(Ctx, Inst)] = constTransformId
  protected[this] val unopTransformId: (Ctx, Unop) => F[(Ctx, Inst)] = {
    case (ctx, u: i32.Clz.type)       => i32ClzTransform(ctx, u)
    case (ctx, u: i32.Ctz.type)       => i32CtzTransform(ctx, u)
    case (ctx, u: i32.Popcnt.type)    => i32PopcntTransform(ctx, u)
    case (ctx, u: i32.Extend8S.type)  => i32Extend8STransform(ctx, u)
    case (ctx, u: i32.Extend16S.type) => i32Extend16STransform(ctx, u)
    case (ctx, u: i64.Clz.type)       => i64ClzTransform(ctx, u)
    case (ctx, u: i64.Ctz.type)       => i64CtzTransform(ctx, u)
    case (ctx, u: i64.Popcnt.type)    => i64PopcntTransform(ctx, u)
    case (ctx, u: i64.Extend8S.type)  => i64Extend8STransform(ctx, u)
    case (ctx, u: i64.Extend16S.type) => i64Extend16STransform(ctx, u)
    case (ctx, u: i64.Extend32S.type) => i64Extend32STransform(ctx, u)
    case (ctx, u: f32.Abs.type)       => f32AbsTransform(ctx, u)
    case (ctx, u: f32.Neg.type)       => f32NegTransform(ctx, u)
    case (ctx, u: f32.Sqrt.type)      => f32SqrtTransform(ctx, u)
    case (ctx, u: f32.Ceil.type)      => f32CeilTransform(ctx, u)
    case (ctx, u: f32.Floor.type)     => f32FloorTransform(ctx, u)
    case (ctx, u: f32.Trunc.type)     => f32TruncTransform(ctx, u)
    case (ctx, u: f32.Nearest.type)   => f32NearestTransform(ctx, u)
    case (ctx, u: f64.Abs.type)       => f64AbsTransform(ctx, u)
    case (ctx, u: f64.Neg.type)       => f64NegTransform(ctx, u)
    case (ctx, u: f64.Sqrt.type)      => f64SqrtTransform(ctx, u)
    case (ctx, u: f64.Ceil.type)      => f64CeilTransform(ctx, u)
    case (ctx, u: f64.Floor.type)     => f64FloorTransform(ctx, u)
    case (ctx, u: f64.Trunc.type)     => f64TruncTransform(ctx, u)
    case (ctx, u: f64.Nearest.type)   => f64NearestTransform(ctx, u)
  }
  val unopTransform: (Ctx, Unop) => F[(Ctx, Inst)] = unopTransformId
  protected[this] val binopTransformId: (Ctx, Binop) => F[(Ctx, Inst)] = {
    case (ctx, b: i32.Add.type)      => i32AddTransform(ctx, b)
    case (ctx, b: i32.Sub.type)      => i32SubTransform(ctx, b)
    case (ctx, b: i32.Mul.type)      => i32MulTransform(ctx, b)
    case (ctx, b: i32.DivS.type)     => i32DivSTransform(ctx, b)
    case (ctx, b: i32.DivU.type)     => i32DivUTransform(ctx, b)
    case (ctx, b: i32.RemS.type)     => i32RemSTransform(ctx, b)
    case (ctx, b: i32.RemU.type)     => i32RemUTransform(ctx, b)
    case (ctx, b: i32.And.type)      => i32AndTransform(ctx, b)
    case (ctx, b: i32.Or.type)       => i32OrTransform(ctx, b)
    case (ctx, b: i32.Xor.type)      => i32XorTransform(ctx, b)
    case (ctx, b: i32.Shl.type)      => i32ShlTransform(ctx, b)
    case (ctx, b: i32.ShrS.type)     => i32ShrSTransform(ctx, b)
    case (ctx, b: i32.ShrU.type)     => i32ShrUTransform(ctx, b)
    case (ctx, b: i32.Rotl.type)     => i32RotlTransform(ctx, b)
    case (ctx, b: i32.Rotr.type)     => i32RotrTransform(ctx, b)
    case (ctx, b: i64.Add.type)      => i64AddTransform(ctx, b)
    case (ctx, b: i64.Sub.type)      => i64SubTransform(ctx, b)
    case (ctx, b: i64.Mul.type)      => i64MulTransform(ctx, b)
    case (ctx, b: i64.DivS.type)     => i64DivSTransform(ctx, b)
    case (ctx, b: i64.DivU.type)     => i64DivUTransform(ctx, b)
    case (ctx, b: i64.RemS.type)     => i64RemSTransform(ctx, b)
    case (ctx, b: i64.RemU.type)     => i64RemUTransform(ctx, b)
    case (ctx, b: i64.And.type)      => i64AndTransform(ctx, b)
    case (ctx, b: i64.Or.type)       => i64OrTransform(ctx, b)
    case (ctx, b: i64.Xor.type)      => i64XorTransform(ctx, b)
    case (ctx, b: i64.Shl.type)      => i64ShlTransform(ctx, b)
    case (ctx, b: i64.ShrS.type)     => i64ShrSTransform(ctx, b)
    case (ctx, b: i64.ShrU.type)     => i64ShrUTransform(ctx, b)
    case (ctx, b: i64.Rotl.type)     => i64RotlTransform(ctx, b)
    case (ctx, b: i64.Rotr.type)     => i64RotrTransform(ctx, b)
    case (ctx, b: f32.Add.type)      => f32AddTransform(ctx, b)
    case (ctx, b: f32.Sub.type)      => f32SubTransform(ctx, b)
    case (ctx, b: f32.Mul.type)      => f32MulTransform(ctx, b)
    case (ctx, b: f32.Div.type)      => f32DivTransform(ctx, b)
    case (ctx, b: f32.Min.type)      => f32MinTransform(ctx, b)
    case (ctx, b: f32.Max.type)      => f32MaxTransform(ctx, b)
    case (ctx, b: f32.Copysign.type) => f32CopysignTransform(ctx, b)
    case (ctx, b: f64.Add.type)      => f64AddTransform(ctx, b)
    case (ctx, b: f64.Sub.type)      => f64SubTransform(ctx, b)
    case (ctx, b: f64.Mul.type)      => f64MulTransform(ctx, b)
    case (ctx, b: f64.Div.type)      => f64DivTransform(ctx, b)
    case (ctx, b: f64.Min.type)      => f64MinTransform(ctx, b)
    case (ctx, b: f64.Max.type)      => f64MaxTransform(ctx, b)
    case (ctx, b: f64.Copysign.type) => f64CopysignTransform(ctx, b)
  }
  val binopTransform: (Ctx, Binop) => F[(Ctx, Inst)] = binopTransformId
  protected[this] val testopTransformId: (Ctx, Testop) => F[(Ctx, Inst)] = {
    case (ctx, t: i32.Eqz.type) => i32EqzTransform(ctx, t)
    case (ctx, t: i64.Eqz.type) => i64EqzTransform(ctx, t)
  }
  val testopTransform: (Ctx, Testop) => F[(Ctx, Inst)] = testopTransformId
  protected[this] val convertopTransformId: (Ctx, Convertop) => F[(Ctx, Inst)] = {
    case (ctx, c: i32.WrapI64.type)        => i32WrapI64Transform(ctx, c)
    case (ctx, c: i32.TruncSF32.type)      => i32TruncSF32Transform(ctx, c)
    case (ctx, c: i32.TruncUF32.type)      => i32TruncUF32Transform(ctx, c)
    case (ctx, c: i32.TruncSF64.type)      => i32TruncSF64Transform(ctx, c)
    case (ctx, c: i32.TruncUF64.type)      => i32TruncUF64Transform(ctx, c)
    case (ctx, c: i32.ReinterpretF32.type) => i32ReinterpretF32Transform(ctx, c)
    case (ctx, c: i64.ExtendSI32.type)     => i64ExtendSI32Transform(ctx, c)
    case (ctx, c: i64.ExtendUI32.type)     => i64ExtendUI32Transform(ctx, c)
    case (ctx, c: i64.TruncSF32.type)      => i64TruncSF32Transform(ctx, c)
    case (ctx, c: i64.TruncUF32.type)      => i64TruncUF32Transform(ctx, c)
    case (ctx, c: i64.TruncSF64.type)      => i64TruncSF64Transform(ctx, c)
    case (ctx, c: i64.TruncUF64.type)      => i64TruncUF64Transform(ctx, c)
    case (ctx, c: i64.ReinterpretF64.type) => i64ReinterpretF64Transform(ctx, c)
    case (ctx, c: f32.DemoteF64.type)      => f32DemoteF64Transform(ctx, c)
    case (ctx, c: f32.ConvertSI32.type)    => f32ConvertSI32Transform(ctx, c)
    case (ctx, c: f32.ConvertUI32.type)    => f32ConvertUI32Transform(ctx, c)
    case (ctx, c: f32.ConvertSI64.type)    => f32ConvertSI64Transform(ctx, c)
    case (ctx, c: f32.ConvertUI64.type)    => f32ConvertUI64Transform(ctx, c)
    case (ctx, c: f32.ReinterpretI32.type) => f32ReinterpretI32Transform(ctx, c)
    case (ctx, c: f64.PromoteF32.type)     => f64PromoteF32Transform(ctx, c)
    case (ctx, c: f64.ConvertSI32.type)    => f64ConvertSI32Transform(ctx, c)
    case (ctx, c: f64.ConvertUI32.type)    => f64ConvertUI32Transform(ctx, c)
    case (ctx, c: f64.ConvertSI64.type)    => f64ConvertSI64Transform(ctx, c)
    case (ctx, c: f64.ConvertUI64.type)    => f64ConvertUI64Transform(ctx, c)
    case (ctx, c: f64.ReinterpretI64.type) => f64ReinterpretI64Transform(ctx, c)
  }
  val convertopTransform: (Ctx, Convertop) => F[(Ctx, Inst)] = convertopTransformId
  protected[this] val satConvertopTransformId: (Ctx, SatConvertop) => F[(Ctx, Inst)] = {
    case (ctx, c: i32.TruncSatSF32.type) => i32TruncSatSF32Transform(ctx, c)
    case (ctx, c: i32.TruncSatUF32.type) => i32TruncSatUF32Transform(ctx, c)
    case (ctx, c: i32.TruncSatSF64.type) => i32TruncSatSF64Transform(ctx, c)
    case (ctx, c: i32.TruncSatUF64.type) => i32TruncSatUF64Transform(ctx, c)
    case (ctx, c: i64.TruncSatSF32.type) => i64TruncSatSF32Transform(ctx, c)
    case (ctx, c: i64.TruncSatUF32.type) => i64TruncSatUF32Transform(ctx, c)
    case (ctx, c: i64.TruncSatSF64.type) => i64TruncSatSF64Transform(ctx, c)
    case (ctx, c: i64.TruncSatUF64.type) => i64TruncSatUF64Transform(ctx, c)
  }
  val satConvertopTransform: (Ctx, SatConvertop) => F[(Ctx, Inst)] = satConvertopTransformId
  protected[this] val memoryInstTransformId: (Ctx, MemoryInst) => F[(Ctx, Inst)] = {
    case (ctx, m @ Load(_, _, _))      => loadInstTransform(ctx, m)
    case (ctx, m @ LoadN(_, _, _, _))  => loadNInstTransform(ctx, m)
    case (ctx, m @ Store(_, _, _))     => storeInstTransform(ctx, m)
    case (ctx, m @ StoreN(_, _, _, _)) => storeNInstTransform(ctx, m)
  }
  val memoryInstTransform: (Ctx, MemoryInst) => F[(Ctx, Inst)] = memoryInstTransformId
  protected[this] val loadInstTransformId: (Ctx, LoadInst) => F[(Ctx, Inst)] = {
    case (ctx, l @ i32.Load(_, _)) => i32LoadTransform(ctx, l)
    case (ctx, l @ i64.Load(_, _)) => i64LoadTransform(ctx, l)
    case (ctx, l @ f32.Load(_, _)) => f32LoadTransform(ctx, l)
    case (ctx, l @ f64.Load(_, _)) => f64LoadTransform(ctx, l)
  }
  val loadInstTransform: (Ctx, LoadInst) => F[(Ctx, Inst)] = loadInstTransformId
  protected[this] val loadNInstTransformId: (Ctx, LoadNInst) => F[(Ctx, Inst)] = {
    case (ctx, l @ i32.Load8S(_, _))  => i32Load8STransform(ctx, l)
    case (ctx, l @ i32.Load8U(_, _))  => i32Load8UTransform(ctx, l)
    case (ctx, l @ i32.Load16S(_, _)) => i32Load16STransform(ctx, l)
    case (ctx, l @ i32.Load16U(_, _)) => i32Load16UTransform(ctx, l)
    case (ctx, l @ i64.Load8S(_, _))  => i64Load8STransform(ctx, l)
    case (ctx, l @ i64.Load8U(_, _))  => i64Load8UTransform(ctx, l)
    case (ctx, l @ i64.Load16S(_, _)) => i64Load16STransform(ctx, l)
    case (ctx, l @ i64.Load16U(_, _)) => i64Load16UTransform(ctx, l)
    case (ctx, l @ i64.Load32S(_, _)) => i64Load32STransform(ctx, l)
    case (ctx, l @ i64.Load32U(_, _)) => i64Load32UTransform(ctx, l)
  }
  val loadNInstTransform: (Ctx, LoadNInst) => F[(Ctx, Inst)] = loadNInstTransformId
  protected[this] val storeInstTransformId: (Ctx, StoreInst) => F[(Ctx, Inst)] = {
    case (ctx, l @ i32.Store(_, _)) => i32StoreTransform(ctx, l)
    case (ctx, l @ i64.Store(_, _)) => i64StoreTransform(ctx, l)
    case (ctx, l @ f32.Store(_, _)) => f32StoreTransform(ctx, l)
    case (ctx, l @ f64.Store(_, _)) => f64StoreTransform(ctx, l)
  }
  val storeInstTransform: (Ctx, StoreInst) => F[(Ctx, Inst)] = storeInstTransformId
  protected[this] val storeNInstTransformId: (Ctx, StoreNInst) => F[(Ctx, Inst)] = {
    case (ctx, l @ i32.Store8(_, _))  => i32Store8Transform(ctx, l)
    case (ctx, l @ i32.Store16(_, _)) => i32Store16Transform(ctx, l)
    case (ctx, l @ i64.Store8(_, _))  => i64Store8Transform(ctx, l)
    case (ctx, l @ i64.Store16(_, _)) => i64Store16Transform(ctx, l)
    case (ctx, l @ i64.Store32(_, _)) => i64Store32Transform(ctx, l)
  }
  val storeNInstTransform: (Ctx, StoreNInst) => F[(Ctx, Inst)] = storeNInstTransformId
  protected[this] val relopTransformId: (Ctx, Relop) => F[(Ctx, Inst)] = {
    case (ctx, r: i32.Eq.type)  => i32EqTransform(ctx, r)
    case (ctx, r: i32.Ne.type)  => i32NeTransform(ctx, r)
    case (ctx, r: i32.LtS.type) => i32LtSTransform(ctx, r)
    case (ctx, r: i32.LtU.type) => i32LtUTransform(ctx, r)
    case (ctx, r: i32.GtS.type) => i32GtSTransform(ctx, r)
    case (ctx, r: i32.GtU.type) => i32GtUTransform(ctx, r)
    case (ctx, r: i32.LeS.type) => i32LeSTransform(ctx, r)
    case (ctx, r: i32.LeU.type) => i32LeUTransform(ctx, r)
    case (ctx, r: i32.GeS.type) => i32GeSTransform(ctx, r)
    case (ctx, r: i32.GeU.type) => i32GeUTransform(ctx, r)
    case (ctx, r: i64.Eq.type)  => i64EqTransform(ctx, r)
    case (ctx, r: i64.Ne.type)  => i64NeTransform(ctx, r)
    case (ctx, r: i64.LtS.type) => i64LtSTransform(ctx, r)
    case (ctx, r: i64.LtU.type) => i64LtUTransform(ctx, r)
    case (ctx, r: i64.GtS.type) => i64GtSTransform(ctx, r)
    case (ctx, r: i64.GtU.type) => i64GtUTransform(ctx, r)
    case (ctx, r: i64.LeS.type) => i64LeSTransform(ctx, r)
    case (ctx, r: i64.LeU.type) => i64LeUTransform(ctx, r)
    case (ctx, r: i64.GeS.type) => i64GeSTransform(ctx, r)
    case (ctx, r: i64.GeU.type) => i64GeUTransform(ctx, r)
    case (ctx, r: f32.Eq.type)  => f32EqTransform(ctx, r)
    case (ctx, r: f32.Ne.type)  => f32NeTransform(ctx, r)
    case (ctx, r: f32.Lt.type)  => f32LtTransform(ctx, r)
    case (ctx, r: f32.Gt.type)  => f32GtTransform(ctx, r)
    case (ctx, r: f32.Le.type)  => f32LeTransform(ctx, r)
    case (ctx, r: f32.Ge.type)  => f32GeTransform(ctx, r)
    case (ctx, r: f64.Eq.type)  => f64EqTransform(ctx, r)
    case (ctx, r: f64.Ne.type)  => f64NeTransform(ctx, r)
    case (ctx, r: f64.Lt.type)  => f64LtTransform(ctx, r)
    case (ctx, r: f64.Gt.type)  => f64GtTransform(ctx, r)
    case (ctx, r: f64.Le.type)  => f64LeTransform(ctx, r)
    case (ctx, r: f64.Ge.type)  => f64GeTransform(ctx, r)
  }
  val relopTransform: (Ctx, Relop) => F[(Ctx, Inst)] = relopTransformId
  protected[this] val varInstTransformId: (Ctx, VarInst) => F[(Ctx, Inst)] = {
    case (ctx, v @ LocalGet(_))  => localGetTransform(ctx, v)
    case (ctx, v @ LocalSet(_))  => localSetTransform(ctx, v)
    case (ctx, v @ LocalTee(_))  => localTeeTransform(ctx, v)
    case (ctx, v @ GlobalGet(_)) => globalGetTransform(ctx, v)
    case (ctx, v @ GlobalSet(_)) => globalSetTransform(ctx, v)
  }
  val varInstTransform: (Ctx, VarInst) => F[(Ctx, Inst)] = varInstTransformId
  val i32ConstTransform: (Ctx, i32.Const) => F[(Ctx, Inst)] = id
  val i32ClzTransform: (Ctx, i32.Clz.type) => F[(Ctx, Inst)] = id
  val i32CtzTransform: (Ctx, i32.Ctz.type) => F[(Ctx, Inst)] = id
  val i32PopcntTransform: (Ctx, i32.Popcnt.type) => F[(Ctx, Inst)] = id
  val i32Extend8STransform: (Ctx, i32.Extend8S.type) => F[(Ctx, Inst)] = id
  val i32Extend16STransform: (Ctx, i32.Extend16S.type) => F[(Ctx, Inst)] = id
  val i32AddTransform: (Ctx, i32.Add.type) => F[(Ctx, Inst)] = id
  val i32SubTransform: (Ctx, i32.Sub.type) => F[(Ctx, Inst)] = id
  val i32MulTransform: (Ctx, i32.Mul.type) => F[(Ctx, Inst)] = id
  val i32DivSTransform: (Ctx, i32.DivS.type) => F[(Ctx, Inst)] = id
  val i32DivUTransform: (Ctx, i32.DivU.type) => F[(Ctx, Inst)] = id
  val i32RemSTransform: (Ctx, i32.RemS.type) => F[(Ctx, Inst)] = id
  val i32RemUTransform: (Ctx, i32.RemU.type) => F[(Ctx, Inst)] = id
  val i32AndTransform: (Ctx, i32.And.type) => F[(Ctx, Inst)] = id
  val i32OrTransform: (Ctx, i32.Or.type) => F[(Ctx, Inst)] = id
  val i32XorTransform: (Ctx, i32.Xor.type) => F[(Ctx, Inst)] = id
  val i32ShlTransform: (Ctx, i32.Shl.type) => F[(Ctx, Inst)] = id
  val i32ShrSTransform: (Ctx, i32.ShrS.type) => F[(Ctx, Inst)] = id
  val i32ShrUTransform: (Ctx, i32.ShrU.type) => F[(Ctx, Inst)] = id
  val i32RotlTransform: (Ctx, i32.Rotl.type) => F[(Ctx, Inst)] = id
  val i32RotrTransform: (Ctx, i32.Rotr.type) => F[(Ctx, Inst)] = id
  val i32EqzTransform: (Ctx, i32.Eqz.type) => F[(Ctx, Inst)] = id
  val i32EqTransform: (Ctx, i32.Eq.type) => F[(Ctx, Inst)] = id
  val i32NeTransform: (Ctx, i32.Ne.type) => F[(Ctx, Inst)] = id
  val i32LtSTransform: (Ctx, i32.LtS.type) => F[(Ctx, Inst)] = id
  val i32LtUTransform: (Ctx, i32.LtU.type) => F[(Ctx, Inst)] = id
  val i32GtSTransform: (Ctx, i32.GtS.type) => F[(Ctx, Inst)] = id
  val i32GtUTransform: (Ctx, i32.GtU.type) => F[(Ctx, Inst)] = id
  val i32LeSTransform: (Ctx, i32.LeS.type) => F[(Ctx, Inst)] = id
  val i32LeUTransform: (Ctx, i32.LeU.type) => F[(Ctx, Inst)] = id
  val i32GeSTransform: (Ctx, i32.GeS.type) => F[(Ctx, Inst)] = id
  val i32GeUTransform: (Ctx, i32.GeU.type) => F[(Ctx, Inst)] = id
  val i32WrapI64Transform: (Ctx, i32.WrapI64.type) => F[(Ctx, Inst)] = id
  val i32TruncSF32Transform: (Ctx, i32.TruncSF32.type) => F[(Ctx, Inst)] = id
  val i32TruncUF32Transform: (Ctx, i32.TruncUF32.type) => F[(Ctx, Inst)] = id
  val i32TruncSF64Transform: (Ctx, i32.TruncSF64.type) => F[(Ctx, Inst)] = id
  val i32TruncUF64Transform: (Ctx, i32.TruncUF64.type) => F[(Ctx, Inst)] = id
  val i32TruncSatSF32Transform: (Ctx, i32.TruncSatSF32.type) => F[(Ctx, Inst)] = id
  val i32TruncSatUF32Transform: (Ctx, i32.TruncSatUF32.type) => F[(Ctx, Inst)] = id
  val i32TruncSatSF64Transform: (Ctx, i32.TruncSatSF64.type) => F[(Ctx, Inst)] = id
  val i32TruncSatUF64Transform: (Ctx, i32.TruncSatUF64.type) => F[(Ctx, Inst)] = id
  val i32ReinterpretF32Transform: (Ctx, i32.ReinterpretF32.type) => F[(Ctx, Inst)] = id
  val i32LoadTransform: (Ctx, i32.Load) => F[(Ctx, Inst)] = id
  val i32StoreTransform: (Ctx, i32.Store) => F[(Ctx, Inst)] = id
  val i32Load8STransform: (Ctx, i32.Load8S) => F[(Ctx, Inst)] = id
  val i32Load8UTransform: (Ctx, i32.Load8U) => F[(Ctx, Inst)] = id
  val i32Load16STransform: (Ctx, i32.Load16S) => F[(Ctx, Inst)] = id
  val i32Load16UTransform: (Ctx, i32.Load16U) => F[(Ctx, Inst)] = id
  val i32Store8Transform: (Ctx, i32.Store8) => F[(Ctx, Inst)] = id
  val i32Store16Transform: (Ctx, i32.Store16) => F[(Ctx, Inst)] = id
  val i64ConstTransform: (Ctx, i64.Const) => F[(Ctx, Inst)] = id
  val i64ClzTransform: (Ctx, i64.Clz.type) => F[(Ctx, Inst)] = id
  val i64CtzTransform: (Ctx, i64.Ctz.type) => F[(Ctx, Inst)] = id
  val i64PopcntTransform: (Ctx, i64.Popcnt.type) => F[(Ctx, Inst)] = id
  val i64Extend8STransform: (Ctx, i64.Extend8S.type) => F[(Ctx, Inst)] = id
  val i64Extend16STransform: (Ctx, i64.Extend16S.type) => F[(Ctx, Inst)] = id
  val i64Extend32STransform: (Ctx, i64.Extend32S.type) => F[(Ctx, Inst)] = id
  val i64AddTransform: (Ctx, i64.Add.type) => F[(Ctx, Inst)] = id
  val i64SubTransform: (Ctx, i64.Sub.type) => F[(Ctx, Inst)] = id
  val i64MulTransform: (Ctx, i64.Mul.type) => F[(Ctx, Inst)] = id
  val i64DivSTransform: (Ctx, i64.DivS.type) => F[(Ctx, Inst)] = id
  val i64DivUTransform: (Ctx, i64.DivU.type) => F[(Ctx, Inst)] = id
  val i64RemSTransform: (Ctx, i64.RemS.type) => F[(Ctx, Inst)] = id
  val i64RemUTransform: (Ctx, i64.RemU.type) => F[(Ctx, Inst)] = id
  val i64AndTransform: (Ctx, i64.And.type) => F[(Ctx, Inst)] = id
  val i64OrTransform: (Ctx, i64.Or.type) => F[(Ctx, Inst)] = id
  val i64XorTransform: (Ctx, i64.Xor.type) => F[(Ctx, Inst)] = id
  val i64ShlTransform: (Ctx, i64.Shl.type) => F[(Ctx, Inst)] = id
  val i64ShrSTransform: (Ctx, i64.ShrS.type) => F[(Ctx, Inst)] = id
  val i64ShrUTransform: (Ctx, i64.ShrU.type) => F[(Ctx, Inst)] = id
  val i64RotlTransform: (Ctx, i64.Rotl.type) => F[(Ctx, Inst)] = id
  val i64RotrTransform: (Ctx, i64.Rotr.type) => F[(Ctx, Inst)] = id
  val i64EqzTransform: (Ctx, i64.Eqz.type) => F[(Ctx, Inst)] = id
  val i64EqTransform: (Ctx, i64.Eq.type) => F[(Ctx, Inst)] = id
  val i64NeTransform: (Ctx, i64.Ne.type) => F[(Ctx, Inst)] = id
  val i64LtSTransform: (Ctx, i64.LtS.type) => F[(Ctx, Inst)] = id
  val i64LtUTransform: (Ctx, i64.LtU.type) => F[(Ctx, Inst)] = id
  val i64GtSTransform: (Ctx, i64.GtS.type) => F[(Ctx, Inst)] = id
  val i64GtUTransform: (Ctx, i64.GtU.type) => F[(Ctx, Inst)] = id
  val i64LeSTransform: (Ctx, i64.LeS.type) => F[(Ctx, Inst)] = id
  val i64LeUTransform: (Ctx, i64.LeU.type) => F[(Ctx, Inst)] = id
  val i64GeSTransform: (Ctx, i64.GeS.type) => F[(Ctx, Inst)] = id
  val i64GeUTransform: (Ctx, i64.GeU.type) => F[(Ctx, Inst)] = id
  val i64ExtendSI32Transform: (Ctx, i64.ExtendSI32.type) => F[(Ctx, Inst)] = id
  val i64ExtendUI32Transform: (Ctx, i64.ExtendUI32.type) => F[(Ctx, Inst)] = id
  val i64TruncSF32Transform: (Ctx, i64.TruncSF32.type) => F[(Ctx, Inst)] = id
  val i64TruncUF32Transform: (Ctx, i64.TruncUF32.type) => F[(Ctx, Inst)] = id
  val i64TruncSF64Transform: (Ctx, i64.TruncSF64.type) => F[(Ctx, Inst)] = id
  val i64TruncUF64Transform: (Ctx, i64.TruncUF64.type) => F[(Ctx, Inst)] = id
  val i64TruncSatSF32Transform: (Ctx, i64.TruncSatSF32.type) => F[(Ctx, Inst)] = id
  val i64TruncSatUF32Transform: (Ctx, i64.TruncSatUF32.type) => F[(Ctx, Inst)] = id
  val i64TruncSatSF64Transform: (Ctx, i64.TruncSatSF64.type) => F[(Ctx, Inst)] = id
  val i64TruncSatUF64Transform: (Ctx, i64.TruncSatUF64.type) => F[(Ctx, Inst)] = id
  val i64ReinterpretF64Transform: (Ctx, i64.ReinterpretF64.type) => F[(Ctx, Inst)] = id
  val i64LoadTransform: (Ctx, i64.Load) => F[(Ctx, Inst)] = id
  val i64StoreTransform: (Ctx, i64.Store) => F[(Ctx, Inst)] = id
  val i64Load8STransform: (Ctx, i64.Load8S) => F[(Ctx, Inst)] = id
  val i64Load8UTransform: (Ctx, i64.Load8U) => F[(Ctx, Inst)] = id
  val i64Load16STransform: (Ctx, i64.Load16S) => F[(Ctx, Inst)] = id
  val i64Load16UTransform: (Ctx, i64.Load16U) => F[(Ctx, Inst)] = id
  val i64Load32STransform: (Ctx, i64.Load32S) => F[(Ctx, Inst)] = id
  val i64Load32UTransform: (Ctx, i64.Load32U) => F[(Ctx, Inst)] = id
  val i64Store8Transform: (Ctx, i64.Store8) => F[(Ctx, Inst)] = id
  val i64Store16Transform: (Ctx, i64.Store16) => F[(Ctx, Inst)] = id
  val i64Store32Transform: (Ctx, i64.Store32) => F[(Ctx, Inst)] = id
  val f32ConstTransform: (Ctx, f32.Const) => F[(Ctx, Inst)] = id
  val f32AbsTransform: (Ctx, f32.Abs.type) => F[(Ctx, Inst)] = id
  val f32NegTransform: (Ctx, f32.Neg.type) => F[(Ctx, Inst)] = id
  val f32SqrtTransform: (Ctx, f32.Sqrt.type) => F[(Ctx, Inst)] = id
  val f32CeilTransform: (Ctx, f32.Ceil.type) => F[(Ctx, Inst)] = id
  val f32FloorTransform: (Ctx, f32.Floor.type) => F[(Ctx, Inst)] = id
  val f32TruncTransform: (Ctx, f32.Trunc.type) => F[(Ctx, Inst)] = id
  val f32NearestTransform: (Ctx, f32.Nearest.type) => F[(Ctx, Inst)] = id
  val f32AddTransform: (Ctx, f32.Add.type) => F[(Ctx, Inst)] = id
  val f32SubTransform: (Ctx, f32.Sub.type) => F[(Ctx, Inst)] = id
  val f32MulTransform: (Ctx, f32.Mul.type) => F[(Ctx, Inst)] = id
  val f32DivTransform: (Ctx, f32.Div.type) => F[(Ctx, Inst)] = id
  val f32MinTransform: (Ctx, f32.Min.type) => F[(Ctx, Inst)] = id
  val f32MaxTransform: (Ctx, f32.Max.type) => F[(Ctx, Inst)] = id
  val f32CopysignTransform: (Ctx, f32.Copysign.type) => F[(Ctx, Inst)] = id
  val f32EqTransform: (Ctx, f32.Eq.type) => F[(Ctx, Inst)] = id
  val f32NeTransform: (Ctx, f32.Ne.type) => F[(Ctx, Inst)] = id
  val f32LtTransform: (Ctx, f32.Lt.type) => F[(Ctx, Inst)] = id
  val f32GtTransform: (Ctx, f32.Gt.type) => F[(Ctx, Inst)] = id
  val f32LeTransform: (Ctx, f32.Le.type) => F[(Ctx, Inst)] = id
  val f32GeTransform: (Ctx, f32.Ge.type) => F[(Ctx, Inst)] = id
  val f32DemoteF64Transform: (Ctx, f32.DemoteF64.type) => F[(Ctx, Inst)] = id
  val f32ConvertSI32Transform: (Ctx, f32.ConvertSI32.type) => F[(Ctx, Inst)] = id
  val f32ConvertUI32Transform: (Ctx, f32.ConvertUI32.type) => F[(Ctx, Inst)] = id
  val f32ConvertSI64Transform: (Ctx, f32.ConvertSI64.type) => F[(Ctx, Inst)] = id
  val f32ConvertUI64Transform: (Ctx, f32.ConvertUI64.type) => F[(Ctx, Inst)] = id
  val f32ReinterpretI32Transform: (Ctx, f32.ReinterpretI32.type) => F[(Ctx, Inst)] = id
  val f32LoadTransform: (Ctx, f32.Load) => F[(Ctx, Inst)] = id
  val f32StoreTransform: (Ctx, f32.Store) => F[(Ctx, Inst)] = id
  val f64ConstTransform: (Ctx, f64.Const) => F[(Ctx, Inst)] = id
  val f64AbsTransform: (Ctx, f64.Abs.type) => F[(Ctx, Inst)] = id
  val f64NegTransform: (Ctx, f64.Neg.type) => F[(Ctx, Inst)] = id
  val f64SqrtTransform: (Ctx, f64.Sqrt.type) => F[(Ctx, Inst)] = id
  val f64CeilTransform: (Ctx, f64.Ceil.type) => F[(Ctx, Inst)] = id
  val f64FloorTransform: (Ctx, f64.Floor.type) => F[(Ctx, Inst)] = id
  val f64TruncTransform: (Ctx, f64.Trunc.type) => F[(Ctx, Inst)] = id
  val f64NearestTransform: (Ctx, f64.Nearest.type) => F[(Ctx, Inst)] = id
  val f64AddTransform: (Ctx, f64.Add.type) => F[(Ctx, Inst)] = id
  val f64SubTransform: (Ctx, f64.Sub.type) => F[(Ctx, Inst)] = id
  val f64MulTransform: (Ctx, f64.Mul.type) => F[(Ctx, Inst)] = id
  val f64DivTransform: (Ctx, f64.Div.type) => F[(Ctx, Inst)] = id
  val f64MinTransform: (Ctx, f64.Min.type) => F[(Ctx, Inst)] = id
  val f64MaxTransform: (Ctx, f64.Max.type) => F[(Ctx, Inst)] = id
  val f64CopysignTransform: (Ctx, f64.Copysign.type) => F[(Ctx, Inst)] = id
  val f64EqTransform: (Ctx, f64.Eq.type) => F[(Ctx, Inst)] = id
  val f64NeTransform: (Ctx, f64.Ne.type) => F[(Ctx, Inst)] = id
  val f64LtTransform: (Ctx, f64.Lt.type) => F[(Ctx, Inst)] = id
  val f64GtTransform: (Ctx, f64.Gt.type) => F[(Ctx, Inst)] = id
  val f64LeTransform: (Ctx, f64.Le.type) => F[(Ctx, Inst)] = id
  val f64GeTransform: (Ctx, f64.Ge.type) => F[(Ctx, Inst)] = id
  val f64PromoteF32Transform: (Ctx, f64.PromoteF32.type) => F[(Ctx, Inst)] = id
  val f64ConvertSI32Transform: (Ctx, f64.ConvertSI32.type) => F[(Ctx, Inst)] = id
  val f64ConvertUI32Transform: (Ctx, f64.ConvertUI32.type) => F[(Ctx, Inst)] = id
  val f64ConvertSI64Transform: (Ctx, f64.ConvertSI64.type) => F[(Ctx, Inst)] = id
  val f64ConvertUI64Transform: (Ctx, f64.ConvertUI64.type) => F[(Ctx, Inst)] = id
  val f64ReinterpretI64Transform: (Ctx, f64.ReinterpretI64.type) => F[(Ctx, Inst)] = id
  val f64LoadTransform: (Ctx, f64.Load) => F[(Ctx, Inst)] = id
  val f64StoreTransform: (Ctx, f64.Store) => F[(Ctx, Inst)] = id
  val dropTransform: (Ctx, Drop.type) => F[(Ctx, Inst)] = id
  val selectTransform: (Ctx, Select.type) => F[(Ctx, Inst)] = id
  val localGetTransform: (Ctx, LocalGet) => F[(Ctx, Inst)] = id
  val localSetTransform: (Ctx, LocalSet) => F[(Ctx, Inst)] = id
  val localTeeTransform: (Ctx, LocalTee) => F[(Ctx, Inst)] = id
  val globalGetTransform: (Ctx, GlobalGet) => F[(Ctx, Inst)] = id
  val globalSetTransform: (Ctx, GlobalSet) => F[(Ctx, Inst)] = id
  val memorySizeTransform: (Ctx, MemorySize.type) => F[(Ctx, Inst)] = id
  val memoryGrowTransform: (Ctx, MemoryGrow.type) => F[(Ctx, Inst)] = id
  val nopTransform: (Ctx, Nop.type) => F[(Ctx, Inst)] = id
  val unreachableTransform: (Ctx, Unreachable.type) => F[(Ctx, Inst)] = id
  val blockTransform: (Ctx, Block) => F[(Ctx, Inst)] = id
  val loopTransform: (Ctx, Loop) => F[(Ctx, Inst)] = id
  val ifTransform: (Ctx, If) => F[(Ctx, Inst)] = id
  val brTransform: (Ctx, Br) => F[(Ctx, Inst)] = id
  val brIfTransform: (Ctx, BrIf) => F[(Ctx, Inst)] = id
  val brTableTransform: (Ctx, BrTable) => F[(Ctx, Inst)] = id
  val returnTransform: (Ctx, Return.type) => F[(Ctx, Inst)] = id
  val callTransform: (Ctx, Call) => F[(Ctx, Inst)] = id
  val callIndirectTransform: (Ctx, CallIndirect) => F[(Ctx, Inst)] = id

  val blockPrepare: (Ctx, Block) => F[Ctx] = fst
  val loopPrepare: (Ctx, Loop) => F[Ctx] = fst
  val ifPrepare: (Ctx, If) => F[Ctx] = fst
  val otherPrepare: (Ctx, Inst) => F[Ctx] = fst

  final def transform(ctx: Ctx, inst: Inst): F[(Ctx, Inst)] = inst match {
    case inst @ AConst(_)          => constTransform(ctx, inst)
    case inst @ Unop(_)            => unopTransform(ctx, inst)
    case inst @ Binop(_)           => binopTransform(ctx, inst)
    case inst @ Testop(_)          => testopTransform(ctx, inst)
    case inst @ Relop(_)           => relopTransform(ctx, inst)
    case inst @ Convertop(_, _)    => convertopTransform(ctx, inst)
    case inst @ SatConvertop(_, _) => satConvertopTransform(ctx, inst)
    case inst @ MemoryInst(_, _)   => memoryInstTransform(ctx, inst)
    case inst: Drop.type           => dropTransform(ctx, inst)
    case inst: Select.type         => selectTransform(ctx, inst)
    case inst @ VarInst(_)         => varInstTransform(ctx, inst)
    case inst: MemorySize.type     => memorySizeTransform(ctx, inst)
    case inst: MemoryGrow.type     => memoryGrowTransform(ctx, inst)
    case inst: Nop.type            => nopTransform(ctx, inst)
    case inst: Unreachable.type    => unreachableTransform(ctx, inst)
    case inst @ Block(_, _)        => blockTransform(ctx, inst)
    case inst @ Loop(_, _)         => loopTransform(ctx, inst)
    case inst @ If(_, _, _)        => ifTransform(ctx, inst)
    case inst @ Br(_)              => brTransform(ctx, inst)
    case inst @ BrIf(_)            => brIfTransform(ctx, inst)
    case inst @ BrTable(_, _)      => brTableTransform(ctx, inst)
    case inst: Return.type         => returnTransform(ctx, inst)
    case inst @ Call(_)            => callTransform(ctx, inst)
    case inst @ CallIndirect(_)    => callIndirectTransform(ctx, inst)
  }

  final def run(ctx: Ctx, inst: Inst): F[(Ctx, Inst)] =
    inst match {
      case b @ Block(tpe, is) =>
        for {
          ctx <- blockPrepare(ctx, b)
          (ctx, is) <- is.mapAccumulateM(ctx)(run(_, _))
          res <- transform(ctx, Block(tpe, is))
        } yield res
      case l @ Loop(tpe, is) =>
        for {
          ctx <- loopPrepare(ctx, l)
          (ctx, is) <- is.mapAccumulateM(ctx)(run(_, _))
          res <- transform(ctx, Loop(tpe, is))
        } yield res
      case i @ If(tpe, ts, es) =>
        for {
          ctx <- ifPrepare(ctx, i)
          (ctx, ts) <- ts.mapAccumulateM(ctx)(run(_, _))
          (ctx, es) <- es.mapAccumulateM(ctx)(run(_, _))
          res <- transform(ctx, If(tpe, ts, es))
        } yield res
      case _ =>
        for {
          ctx <- otherPrepare(ctx, inst)
          res <- transform(ctx, inst)
        } yield res
    }

}

object Transformer {

  implicit def TransformerSemigroup[F[_], Ctx](implicit F: Monad[F]): Semigroup[Transformer[F, Ctx]] =
    new Semigroup[Transformer[F, Ctx]] {
      def combine(first: Transformer[F, Ctx], second: Transformer[F, Ctx]): Transformer[F, Ctx] =
        new Transformer[F, Ctx] {
          override val constTransform: (Ctx, AConst) => F[(Ctx, Inst)] =
            if (first.constTransform == constTransformId)
              second.constTransform
            else if (second.constTransform == constTransformId)
              first.constTransform
            else { (ctx: Ctx, t: AConst) =>
              first.constTransform(ctx, t).flatMap {
                case (ctx, t: AConst) => second.constTransform(ctx, t)
                case (ctx, t)         => second.transform(ctx, t)
              }
            }
          override val unopTransform: (Ctx, Unop) => F[(Ctx, Inst)] =
            if (first.unopTransform == unopTransformId)
              second.unopTransform
            else if (second.unopTransform == unopTransformId)
              first.unopTransform
            else { (ctx: Ctx, t: Unop) =>
              first.unopTransform(ctx, t).flatMap {
                case (ctx, t: Unop) => second.unopTransform(ctx, t)
                case (ctx, t)       => second.transform(ctx, t)
              }
            }
          override val binopTransform: (Ctx, Binop) => F[(Ctx, Inst)] =
            if (first.binopTransform == binopTransformId)
              second.binopTransform
            else if (second.binopTransform == binopTransformId)
              first.binopTransform
            else { (ctx: Ctx, t: Binop) =>
              first.binopTransform(ctx, t).flatMap {
                case (ctx, t: Binop) => second.binopTransform(ctx, t)
                case (ctx, t)        => second.transform(ctx, t)
              }
            }
          override val testopTransform: (Ctx, Testop) => F[(Ctx, Inst)] =
            if (first.testopTransform == testopTransformId)
              second.testopTransform
            else if (second.testopTransform == testopTransformId)
              first.testopTransform
            else { (ctx: Ctx, t: Testop) =>
              first.testopTransform(ctx, t).flatMap {
                case (ctx, t: Testop) => second.testopTransform(ctx, t)
                case (ctx, t)         => second.transform(ctx, t)
              }
            }
          override val convertopTransform: (Ctx, Convertop) => F[(Ctx, Inst)] =
            if (first.convertopTransform == convertopTransformId)
              second.convertopTransform
            else if (second.convertopTransform == convertopTransformId)
              first.convertopTransform
            else { (ctx: Ctx, t: Convertop) =>
              first.convertopTransform(ctx, t).flatMap {
                case (ctx, t: Convertop) => second.convertopTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val satConvertopTransform: (Ctx, SatConvertop) => F[(Ctx, Inst)] =
            if (first.satConvertopTransform == satConvertopTransformId)
              second.satConvertopTransform
            else if (second.satConvertopTransform == satConvertopTransformId)
              first.satConvertopTransform
            else { (ctx: Ctx, t: SatConvertop) =>
              first.satConvertopTransform(ctx, t).flatMap {
                case (ctx, t: SatConvertop) => second.satConvertopTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val memoryInstTransform: (Ctx, MemoryInst) => F[(Ctx, Inst)] =
            if (first.memoryInstTransform == memoryInstTransformId)
              second.memoryInstTransform
            else if (second.memoryInstTransform == memoryInstTransformId)
              first.memoryInstTransform
            else { (ctx: Ctx, t: MemoryInst) =>
              first.memoryInstTransform(ctx, t).flatMap {
                case (ctx, t: MemoryInst) => second.memoryInstTransform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val loadInstTransform: (Ctx, LoadInst) => F[(Ctx, Inst)] =
            if (first.loadInstTransform == loadInstTransformId)
              second.loadInstTransform
            else if (second.loadInstTransform == loadInstTransformId)
              first.loadInstTransform
            else { (ctx: Ctx, t: LoadInst) =>
              first.loadInstTransform(ctx, t).flatMap {
                case (ctx, t: LoadInst) => second.loadInstTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val loadNInstTransform: (Ctx, LoadNInst) => F[(Ctx, Inst)] =
            if (first.loadNInstTransform == loadNInstTransformId)
              second.loadNInstTransform
            else if (second.loadNInstTransform == loadNInstTransformId)
              first.loadNInstTransform
            else { (ctx: Ctx, t: LoadNInst) =>
              first.loadNInstTransform(ctx, t).flatMap {
                case (ctx, t: LoadNInst) => second.loadNInstTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val storeInstTransform: (Ctx, StoreInst) => F[(Ctx, Inst)] =
            if (first.storeInstTransform == storeInstTransformId)
              second.storeInstTransform
            else if (second.storeInstTransform == storeInstTransformId)
              first.storeInstTransform
            else { (ctx: Ctx, t: StoreInst) =>
              first.storeInstTransform(ctx, t).flatMap {
                case (ctx, t: StoreInst) => second.storeInstTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val storeNInstTransform: (Ctx, StoreNInst) => F[(Ctx, Inst)] =
            if (first.storeNInstTransform == storeNInstTransformId)
              second.storeNInstTransform
            else if (second.storeNInstTransform == storeNInstTransformId)
              first.storeNInstTransform
            else { (ctx: Ctx, t: StoreNInst) =>
              first.storeNInstTransform(ctx, t).flatMap {
                case (ctx, t: StoreNInst) => second.storeNInstTransform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val relopTransform: (Ctx, Relop) => F[(Ctx, Inst)] =
            if (first.relopTransform == relopTransformId)
              second.relopTransform
            else if (second.relopTransform == relopTransformId)
              first.relopTransform
            else { (ctx: Ctx, t: Relop) =>
              first.relopTransform(ctx, t).flatMap {
                case (ctx, t: Relop) => second.relopTransform(ctx, t)
                case (ctx, t)        => second.transform(ctx, t)
              }
            }
          override val varInstTransform: (Ctx, VarInst) => F[(Ctx, Inst)] =
            if (first.varInstTransform == varInstTransformId)
              second.varInstTransform
            else if (second.varInstTransform == varInstTransformId)
              first.varInstTransform
            else { (ctx: Ctx, t: VarInst) =>
              first.varInstTransform(ctx, t).flatMap {
                case (ctx, t: VarInst) => second.varInstTransform(ctx, t)
                case (ctx, t)          => second.transform(ctx, t)
              }
            }
          override val i32ConstTransform: (Ctx, i32.Const) => F[(Ctx, Inst)] =
            if (first.i32ConstTransform == id)
              second.i32ConstTransform
            else if (second.i32ConstTransform == id)
              first.i32ConstTransform
            else { (ctx: Ctx, t: i32.Const) =>
              first.i32ConstTransform(ctx, t).flatMap {
                case (ctx, t: i32.Const) => second.i32ConstTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val i32ClzTransform: (Ctx, i32.Clz.type) => F[(Ctx, Inst)] =
            if (first.i32ClzTransform == id)
              second.i32ClzTransform
            else if (second.i32ClzTransform == id)
              first.i32ClzTransform
            else { (ctx: Ctx, t: i32.Clz.type) =>
              first.i32ClzTransform(ctx, t).flatMap {
                case (ctx, t: i32.Clz.type) => second.i32ClzTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32CtzTransform: (Ctx, i32.Ctz.type) => F[(Ctx, Inst)] =
            if (first.i32CtzTransform == id)
              second.i32CtzTransform
            else if (second.i32CtzTransform == id)
              first.i32CtzTransform
            else { (ctx: Ctx, t: i32.Ctz.type) =>
              first.i32CtzTransform(ctx, t).flatMap {
                case (ctx, t: i32.Ctz.type) => second.i32CtzTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32Extend8STransform: (Ctx, i32.Extend8S.type) => F[(Ctx, Inst)] =
            if (first.i32Extend8STransform == id)
              second.i32Extend8STransform
            else if (second.i32Extend8STransform == id)
              first.i32Extend8STransform
            else { (ctx: Ctx, t: i32.Extend8S.type) =>
              first.i32Extend8STransform(ctx, t).flatMap {
                case (ctx, t: i32.Extend8S.type) => second.i32Extend8STransform(ctx, t)
                case (ctx, t)                    => second.transform(ctx, t)
              }
            }
          override val i32Extend16STransform: (Ctx, i32.Extend16S.type) => F[(Ctx, Inst)] =
            if (first.i32Extend16STransform == id)
              second.i32Extend16STransform
            else if (second.i32Extend16STransform == id)
              first.i32Extend16STransform
            else { (ctx: Ctx, t: i32.Extend16S.type) =>
              first.i32Extend16STransform(ctx, t).flatMap {
                case (ctx, t: i32.Extend16S.type) => second.i32Extend16STransform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i32PopcntTransform: (Ctx, i32.Popcnt.type) => F[(Ctx, Inst)] =
            if (first.i32PopcntTransform == id)
              second.i32PopcntTransform
            else if (second.i32PopcntTransform == id)
              first.i32PopcntTransform
            else { (ctx: Ctx, t: i32.Popcnt.type) =>
              first.i32PopcntTransform(ctx, t).flatMap {
                case (ctx, t: i32.Popcnt.type) => second.i32PopcntTransform(ctx, t)
                case (ctx, t)                  => second.transform(ctx, t)
              }
            }
          override val i32AddTransform: (Ctx, i32.Add.type) => F[(Ctx, Inst)] =
            if (first.i32AddTransform == id)
              second.i32AddTransform
            else if (second.i32AddTransform == id)
              first.i32AddTransform
            else { (ctx: Ctx, t: i32.Add.type) =>
              first.i32AddTransform(ctx, t).flatMap {
                case (ctx, t: i32.Add.type) => second.i32AddTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32SubTransform: (Ctx, i32.Sub.type) => F[(Ctx, Inst)] =
            if (first.i32SubTransform == id)
              second.i32SubTransform
            else if (second.i32SubTransform == id)
              first.i32SubTransform
            else { (ctx: Ctx, t: i32.Sub.type) =>
              first.i32SubTransform(ctx, t).flatMap {
                case (ctx, t: i32.Sub.type) => second.i32SubTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32MulTransform: (Ctx, i32.Mul.type) => F[(Ctx, Inst)] =
            if (first.i32MulTransform == id)
              second.i32MulTransform
            else if (second.i32MulTransform == id)
              first.i32MulTransform
            else { (ctx: Ctx, t: i32.Mul.type) =>
              first.i32MulTransform(ctx, t).flatMap {
                case (ctx, t: i32.Mul.type) => second.i32MulTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32DivSTransform: (Ctx, i32.DivS.type) => F[(Ctx, Inst)] =
            if (first.i32DivSTransform == id)
              second.i32DivSTransform
            else if (second.i32DivSTransform == id)
              first.i32DivSTransform
            else { (ctx: Ctx, t: i32.DivS.type) =>
              first.i32DivSTransform(ctx, t).flatMap {
                case (ctx, t: i32.DivS.type) => second.i32DivSTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32DivUTransform: (Ctx, i32.DivU.type) => F[(Ctx, Inst)] =
            if (first.i32DivUTransform == id)
              second.i32DivUTransform
            else if (second.i32DivUTransform == id)
              first.i32DivUTransform
            else { (ctx: Ctx, t: i32.DivU.type) =>
              first.i32DivUTransform(ctx, t).flatMap {
                case (ctx, t: i32.DivU.type) => second.i32DivUTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32RemSTransform: (Ctx, i32.RemS.type) => F[(Ctx, Inst)] =
            if (first.i32RemSTransform == id)
              second.i32RemSTransform
            else if (second.i32RemSTransform == id)
              first.i32RemSTransform
            else { (ctx: Ctx, t: i32.RemS.type) =>
              first.i32RemSTransform(ctx, t).flatMap {
                case (ctx, t: i32.RemS.type) => second.i32RemSTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32RemUTransform: (Ctx, i32.RemU.type) => F[(Ctx, Inst)] =
            if (first.i32RemUTransform == id)
              second.i32RemUTransform
            else if (second.i32RemUTransform == id)
              first.i32RemUTransform
            else { (ctx: Ctx, t: i32.RemU.type) =>
              first.i32RemUTransform(ctx, t).flatMap {
                case (ctx, t: i32.RemU.type) => second.i32RemUTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32AndTransform: (Ctx, i32.And.type) => F[(Ctx, Inst)] =
            if (first.i32AndTransform == id)
              second.i32AndTransform
            else if (second.i32AndTransform == id)
              first.i32AndTransform
            else { (ctx: Ctx, t: i32.And.type) =>
              first.i32AndTransform(ctx, t).flatMap {
                case (ctx, t: i32.And.type) => second.i32AndTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32OrTransform: (Ctx, i32.Or.type) => F[(Ctx, Inst)] =
            if (first.i32OrTransform == id)
              second.i32OrTransform
            else if (second.i32OrTransform == id)
              first.i32OrTransform
            else { (ctx: Ctx, t: i32.Or.type) =>
              first.i32OrTransform(ctx, t).flatMap {
                case (ctx, t: i32.Or.type) => second.i32OrTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i32XorTransform: (Ctx, i32.Xor.type) => F[(Ctx, Inst)] =
            if (first.i32XorTransform == id)
              second.i32XorTransform
            else if (second.i32XorTransform == id)
              first.i32XorTransform
            else { (ctx: Ctx, t: i32.Xor.type) =>
              first.i32XorTransform(ctx, t).flatMap {
                case (ctx, t: i32.Xor.type) => second.i32XorTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32ShlTransform: (Ctx, i32.Shl.type) => F[(Ctx, Inst)] =
            if (first.i32ShlTransform == id)
              second.i32ShlTransform
            else if (second.i32ShlTransform == id)
              first.i32ShlTransform
            else { (ctx: Ctx, t: i32.Shl.type) =>
              first.i32ShlTransform(ctx, t).flatMap {
                case (ctx, t: i32.Shl.type) => second.i32ShlTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32ShrSTransform: (Ctx, i32.ShrS.type) => F[(Ctx, Inst)] =
            if (first.i32ShrSTransform == id)
              second.i32ShrSTransform
            else if (second.i32ShrSTransform == id)
              first.i32ShrSTransform
            else { (ctx: Ctx, t: i32.ShrS.type) =>
              first.i32ShrSTransform(ctx, t).flatMap {
                case (ctx, t: i32.ShrS.type) => second.i32ShrSTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32ShrUTransform: (Ctx, i32.ShrU.type) => F[(Ctx, Inst)] =
            if (first.i32ShrUTransform == id)
              second.i32ShrUTransform
            else if (second.i32ShrUTransform == id)
              first.i32ShrUTransform
            else { (ctx: Ctx, t: i32.ShrU.type) =>
              first.i32ShrUTransform(ctx, t).flatMap {
                case (ctx, t: i32.ShrU.type) => second.i32ShrUTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32RotlTransform: (Ctx, i32.Rotl.type) => F[(Ctx, Inst)] =
            if (first.i32RotlTransform == id)
              second.i32RotlTransform
            else if (second.i32RotlTransform == id)
              first.i32RotlTransform
            else { (ctx: Ctx, t: i32.Rotl.type) =>
              first.i32RotlTransform(ctx, t).flatMap {
                case (ctx, t: i32.Rotl.type) => second.i32RotlTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32RotrTransform: (Ctx, i32.Rotr.type) => F[(Ctx, Inst)] =
            if (first.i32RotrTransform == id)
              second.i32RotrTransform
            else if (second.i32RotrTransform == id)
              first.i32RotrTransform
            else { (ctx: Ctx, t: i32.Rotr.type) =>
              first.i32RotrTransform(ctx, t).flatMap {
                case (ctx, t: i32.Rotr.type) => second.i32RotrTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i32EqzTransform: (Ctx, i32.Eqz.type) => F[(Ctx, Inst)] =
            if (first.i32EqzTransform == id)
              second.i32EqzTransform
            else if (second.i32EqzTransform == id)
              first.i32EqzTransform
            else { (ctx: Ctx, t: i32.Eqz.type) =>
              first.i32EqzTransform(ctx, t).flatMap {
                case (ctx, t: i32.Eqz.type) => second.i32EqzTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32EqTransform: (Ctx, i32.Eq.type) => F[(Ctx, Inst)] =
            if (first.i32EqTransform == id)
              second.i32EqTransform
            else if (second.i32EqTransform == id)
              first.i32EqTransform
            else { (ctx: Ctx, t: i32.Eq.type) =>
              first.i32EqTransform(ctx, t).flatMap {
                case (ctx, t: i32.Eq.type) => second.i32EqTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i32NeTransform: (Ctx, i32.Ne.type) => F[(Ctx, Inst)] =
            if (first.i32NeTransform == id)
              second.i32NeTransform
            else if (second.i32NeTransform == id)
              first.i32NeTransform
            else { (ctx: Ctx, t: i32.Ne.type) =>
              first.i32NeTransform(ctx, t).flatMap {
                case (ctx, t: i32.Ne.type) => second.i32NeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i32LtSTransform: (Ctx, i32.LtS.type) => F[(Ctx, Inst)] =
            if (first.i32LtSTransform == id)
              second.i32LtSTransform
            else if (second.i32LtSTransform == id)
              first.i32LtSTransform
            else { (ctx: Ctx, t: i32.LtS.type) =>
              first.i32LtSTransform(ctx, t).flatMap {
                case (ctx, t: i32.LtS.type) => second.i32LtSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32LtUTransform: (Ctx, i32.LtU.type) => F[(Ctx, Inst)] =
            if (first.i32LtUTransform == id)
              second.i32LtUTransform
            else if (second.i32LtUTransform == id)
              first.i32LtUTransform
            else { (ctx: Ctx, t: i32.LtU.type) =>
              first.i32LtUTransform(ctx, t).flatMap {
                case (ctx, t: i32.LtU.type) => second.i32LtUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32GtSTransform: (Ctx, i32.GtS.type) => F[(Ctx, Inst)] =
            if (first.i32GtSTransform == id)
              second.i32GtSTransform
            else if (second.i32GtSTransform == id)
              first.i32GtSTransform
            else { (ctx: Ctx, t: i32.GtS.type) =>
              first.i32GtSTransform(ctx, t).flatMap {
                case (ctx, t: i32.GtS.type) => second.i32GtSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32GtUTransform: (Ctx, i32.GtU.type) => F[(Ctx, Inst)] =
            if (first.i32GtUTransform == id)
              second.i32GtUTransform
            else if (second.i32GtUTransform == id)
              first.i32GtUTransform
            else { (ctx: Ctx, t: i32.GtU.type) =>
              first.i32GtUTransform(ctx, t).flatMap {
                case (ctx, t: i32.GtU.type) => second.i32GtUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32LeSTransform: (Ctx, i32.LeS.type) => F[(Ctx, Inst)] =
            if (first.i32LeSTransform == id)
              second.i32LeSTransform
            else if (second.i32LeSTransform == id)
              first.i32LeSTransform
            else { (ctx: Ctx, t: i32.LeS.type) =>
              first.i32LeSTransform(ctx, t).flatMap {
                case (ctx, t: i32.LeS.type) => second.i32LeSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32LeUTransform: (Ctx, i32.LeU.type) => F[(Ctx, Inst)] =
            if (first.i32LeUTransform == id)
              second.i32LeUTransform
            else if (second.i32LeUTransform == id)
              first.i32LeUTransform
            else { (ctx: Ctx, t: i32.LeU.type) =>
              first.i32LeUTransform(ctx, t).flatMap {
                case (ctx, t: i32.LeU.type) => second.i32LeUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32GeSTransform: (Ctx, i32.GeS.type) => F[(Ctx, Inst)] =
            if (first.i32GeSTransform == id)
              second.i32GeSTransform
            else if (second.i32GeSTransform == id)
              first.i32GeSTransform
            else { (ctx: Ctx, t: i32.GeS.type) =>
              first.i32GeSTransform(ctx, t).flatMap {
                case (ctx, t: i32.GeS.type) => second.i32GeSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32GeUTransform: (Ctx, i32.GeU.type) => F[(Ctx, Inst)] =
            if (first.i32GeUTransform == id)
              second.i32GeUTransform
            else if (second.i32GeUTransform == id)
              first.i32GeUTransform
            else { (ctx: Ctx, t: i32.GeU.type) =>
              first.i32GeUTransform(ctx, t).flatMap {
                case (ctx, t: i32.GeU.type) => second.i32GeUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i32WrapI64Transform: (Ctx, i32.WrapI64.type) => F[(Ctx, Inst)] =
            if (first.i32WrapI64Transform == id)
              second.i32WrapI64Transform
            else if (second.i32WrapI64Transform == id)
              first.i32WrapI64Transform
            else { (ctx: Ctx, t: i32.WrapI64.type) =>
              first.i32WrapI64Transform(ctx, t).flatMap {
                case (ctx, t: i32.WrapI64.type) => second.i32WrapI64Transform(ctx, t)
                case (ctx, t)                   => second.transform(ctx, t)
              }
            }
          override val i32TruncSF32Transform: (Ctx, i32.TruncSF32.type) => F[(Ctx, Inst)] =
            if (first.i32TruncSF32Transform == id)
              second.i32TruncSF32Transform
            else if (second.i32TruncSF32Transform == id)
              first.i32TruncSF32Transform
            else { (ctx: Ctx, t: i32.TruncSF32.type) =>
              first.i32TruncSF32Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncSF32.type) => second.i32TruncSF32Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i32TruncUF32Transform: (Ctx, i32.TruncUF32.type) => F[(Ctx, Inst)] =
            if (first.i32TruncUF32Transform == id)
              second.i32TruncUF32Transform
            else if (second.i32TruncUF32Transform == id)
              first.i32TruncUF32Transform
            else { (ctx: Ctx, t: i32.TruncUF32.type) =>
              first.i32TruncUF32Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncUF32.type) => second.i32TruncUF32Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i32TruncSF64Transform: (Ctx, i32.TruncSF64.type) => F[(Ctx, Inst)] =
            if (first.i32TruncSF64Transform == id)
              second.i32TruncSF64Transform
            else if (second.i32TruncSF64Transform == id)
              first.i32TruncSF64Transform
            else { (ctx: Ctx, t: i32.TruncSF64.type) =>
              first.i32TruncSF64Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncSF64.type) => second.i32TruncSF64Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i32TruncUF64Transform: (Ctx, i32.TruncUF64.type) => F[(Ctx, Inst)] =
            if (first.i32TruncUF64Transform == id)
              second.i32TruncUF64Transform
            else if (second.i32TruncUF64Transform == id)
              first.i32TruncUF64Transform
            else { (ctx: Ctx, t: i32.TruncUF64.type) =>
              first.i32TruncUF64Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncUF64.type) => second.i32TruncUF64Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i32TruncSatSF32Transform: (Ctx, i32.TruncSatSF32.type) => F[(Ctx, Inst)] =
            if (first.i32TruncSatSF32Transform == id)
              second.i32TruncSatSF32Transform
            else if (second.i32TruncSatSF32Transform == id)
              first.i32TruncSatSF32Transform
            else { (ctx: Ctx, t: i32.TruncSatSF32.type) =>
              first.i32TruncSatSF32Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncSatSF32.type) => second.i32TruncSatSF32Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i32TruncSatUF32Transform: (Ctx, i32.TruncSatUF32.type) => F[(Ctx, Inst)] =
            if (first.i32TruncSatUF32Transform == id)
              second.i32TruncSatUF32Transform
            else if (second.i32TruncSatUF32Transform == id)
              first.i32TruncSatUF32Transform
            else { (ctx: Ctx, t: i32.TruncSatUF32.type) =>
              first.i32TruncSatUF32Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncSatUF32.type) => second.i32TruncSatUF32Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i32TruncSatSF64Transform: (Ctx, i32.TruncSatSF64.type) => F[(Ctx, Inst)] =
            if (first.i32TruncSatSF64Transform == id)
              second.i32TruncSatSF64Transform
            else if (second.i32TruncSatSF64Transform == id)
              first.i32TruncSatSF64Transform
            else { (ctx: Ctx, t: i32.TruncSatSF64.type) =>
              first.i32TruncSatSF64Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncSatSF64.type) => second.i32TruncSatSF64Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i32TruncSatUF64Transform: (Ctx, i32.TruncSatUF64.type) => F[(Ctx, Inst)] =
            if (first.i32TruncSatUF64Transform == id)
              second.i32TruncSatUF64Transform
            else if (second.i32TruncSatUF64Transform == id)
              first.i32TruncSatUF64Transform
            else { (ctx: Ctx, t: i32.TruncSatUF64.type) =>
              first.i32TruncSatUF64Transform(ctx, t).flatMap {
                case (ctx, t: i32.TruncSatUF64.type) => second.i32TruncSatUF64Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i32ReinterpretF32Transform: (Ctx, i32.ReinterpretF32.type) => F[(Ctx, Inst)] =
            if (first.i32ReinterpretF32Transform == id)
              second.i32ReinterpretF32Transform
            else if (second.i32ReinterpretF32Transform == id)
              first.i32ReinterpretF32Transform
            else { (ctx: Ctx, t: i32.ReinterpretF32.type) =>
              first.i32ReinterpretF32Transform(ctx, t).flatMap {
                case (ctx, t: i32.ReinterpretF32.type) => second.i32ReinterpretF32Transform(ctx, t)
                case (ctx, t)                          => second.transform(ctx, t)
              }
            }
          override val i32LoadTransform: (Ctx, i32.Load) => F[(Ctx, Inst)] =
            if (first.i32LoadTransform == id)
              second.i32LoadTransform
            else if (second.i32LoadTransform == id)
              first.i32LoadTransform
            else { (ctx: Ctx, t: i32.Load) =>
              first.i32LoadTransform(ctx, t).flatMap {
                case (ctx, t: i32.Load) => second.i32LoadTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val i32StoreTransform: (Ctx, i32.Store) => F[(Ctx, Inst)] =
            if (first.i32StoreTransform == id)
              second.i32StoreTransform
            else if (second.i32StoreTransform == id)
              first.i32StoreTransform
            else { (ctx: Ctx, t: i32.Store) =>
              first.i32StoreTransform(ctx, t).flatMap {
                case (ctx, t: i32.Store) => second.i32StoreTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val i32Load8STransform: (Ctx, i32.Load8S) => F[(Ctx, Inst)] =
            if (first.i32Load8STransform == id)
              second.i32Load8STransform
            else if (second.i32Load8STransform == id)
              first.i32Load8STransform
            else { (ctx: Ctx, t: i32.Load8S) =>
              first.i32Load8STransform(ctx, t).flatMap {
                case (ctx, t: i32.Load8S) => second.i32Load8STransform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val i32Load8UTransform: (Ctx, i32.Load8U) => F[(Ctx, Inst)] =
            if (first.i32Load8UTransform == id)
              second.i32Load8UTransform
            else if (second.i32Load8UTransform == id)
              first.i32Load8UTransform
            else { (ctx: Ctx, t: i32.Load8U) =>
              first.i32Load8UTransform(ctx, t).flatMap {
                case (ctx, t: i32.Load8U) => second.i32Load8UTransform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val i32Load16STransform: (Ctx, i32.Load16S) => F[(Ctx, Inst)] =
            if (first.i32Load16STransform == id)
              second.i32Load16STransform
            else if (second.i32Load16STransform == id)
              first.i32Load16STransform
            else { (ctx: Ctx, t: i32.Load16S) =>
              first.i32Load16STransform(ctx, t).flatMap {
                case (ctx, t: i32.Load16S) => second.i32Load16STransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i32Load16UTransform: (Ctx, i32.Load16U) => F[(Ctx, Inst)] =
            if (first.i32Load16UTransform == id)
              second.i32Load16UTransform
            else if (second.i32Load16UTransform == id)
              first.i32Load16UTransform
            else { (ctx: Ctx, t: i32.Load16U) =>
              first.i32Load16UTransform(ctx, t).flatMap {
                case (ctx, t: i32.Load16U) => second.i32Load16UTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i32Store8Transform: (Ctx, i32.Store8) => F[(Ctx, Inst)] =
            if (first.i32Store8Transform == id)
              second.i32Store8Transform
            else if (second.i32Store8Transform == id)
              first.i32Store8Transform
            else { (ctx: Ctx, t: i32.Store8) =>
              first.i32Store8Transform(ctx, t).flatMap {
                case (ctx, t: i32.Store8) => second.i32Store8Transform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val i32Store16Transform: (Ctx, i32.Store16) => F[(Ctx, Inst)] =
            if (first.i32Store16Transform == id)
              second.i32Store16Transform
            else if (second.i32Store16Transform == id)
              first.i32Store16Transform
            else { (ctx: Ctx, t: i32.Store16) =>
              first.i32Store16Transform(ctx, t).flatMap {
                case (ctx, t: i32.Store16) => second.i32Store16Transform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64ConstTransform: (Ctx, i64.Const) => F[(Ctx, Inst)] =
            if (first.i64ConstTransform == id)
              second.i64ConstTransform
            else if (second.i64ConstTransform == id)
              first.i64ConstTransform
            else { (ctx: Ctx, t: i64.Const) =>
              first.i64ConstTransform(ctx, t).flatMap {
                case (ctx, t: i64.Const) => second.i64ConstTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val i64ClzTransform: (Ctx, i64.Clz.type) => F[(Ctx, Inst)] =
            if (first.i64ClzTransform == id)
              second.i64ClzTransform
            else if (second.i64ClzTransform == id)
              first.i64ClzTransform
            else { (ctx: Ctx, t: i64.Clz.type) =>
              first.i64ClzTransform(ctx, t).flatMap {
                case (ctx, t: i64.Clz.type) => second.i64ClzTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64CtzTransform: (Ctx, i64.Ctz.type) => F[(Ctx, Inst)] =
            if (first.i64CtzTransform == id)
              second.i64CtzTransform
            else if (second.i64CtzTransform == id)
              first.i64CtzTransform
            else { (ctx: Ctx, t: i64.Ctz.type) =>
              first.i64CtzTransform(ctx, t).flatMap {
                case (ctx, t: i64.Ctz.type) => second.i64CtzTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64PopcntTransform: (Ctx, i64.Popcnt.type) => F[(Ctx, Inst)] =
            if (first.i64PopcntTransform == id)
              second.i64PopcntTransform
            else if (second.i64PopcntTransform == id)
              first.i64PopcntTransform
            else { (ctx: Ctx, t: i64.Popcnt.type) =>
              first.i64PopcntTransform(ctx, t).flatMap {
                case (ctx, t: i64.Popcnt.type) => second.i64PopcntTransform(ctx, t)
                case (ctx, t)                  => second.transform(ctx, t)
              }
            }
          override val i64Extend8STransform: (Ctx, i64.Extend8S.type) => F[(Ctx, Inst)] =
            if (first.i64Extend8STransform == id)
              second.i64Extend8STransform
            else if (second.i64Extend8STransform == id)
              first.i64Extend8STransform
            else { (ctx: Ctx, t: i64.Extend8S.type) =>
              first.i64Extend8STransform(ctx, t).flatMap {
                case (ctx, t: i64.Extend8S.type) => second.i64Extend8STransform(ctx, t)
                case (ctx, t)                    => second.transform(ctx, t)
              }
            }
          override val i64Extend16STransform: (Ctx, i64.Extend16S.type) => F[(Ctx, Inst)] =
            if (first.i64Extend16STransform == id)
              second.i64Extend16STransform
            else if (second.i64Extend16STransform == id)
              first.i64Extend16STransform
            else { (ctx: Ctx, t: i64.Extend16S.type) =>
              first.i64Extend16STransform(ctx, t).flatMap {
                case (ctx, t: i64.Extend16S.type) => second.i64Extend16STransform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i64Extend32STransform: (Ctx, i64.Extend32S.type) => F[(Ctx, Inst)] =
            if (first.i64Extend32STransform == id)
              second.i64Extend32STransform
            else if (second.i64Extend32STransform == id)
              first.i64Extend32STransform
            else { (ctx: Ctx, t: i64.Extend32S.type) =>
              first.i64Extend32STransform(ctx, t).flatMap {
                case (ctx, t: i64.Extend32S.type) => second.i64Extend32STransform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i64AddTransform: (Ctx, i64.Add.type) => F[(Ctx, Inst)] =
            if (first.i64AddTransform == id)
              second.i64AddTransform
            else if (second.i64AddTransform == id)
              first.i64AddTransform
            else { (ctx: Ctx, t: i64.Add.type) =>
              first.i64AddTransform(ctx, t).flatMap {
                case (ctx, t: i64.Add.type) => second.i64AddTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64SubTransform: (Ctx, i64.Sub.type) => F[(Ctx, Inst)] =
            if (first.i64SubTransform == id)
              second.i64SubTransform
            else if (second.i64SubTransform == id)
              first.i64SubTransform
            else { (ctx: Ctx, t: i64.Sub.type) =>
              first.i64SubTransform(ctx, t).flatMap {
                case (ctx, t: i64.Sub.type) => second.i64SubTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64MulTransform: (Ctx, i64.Mul.type) => F[(Ctx, Inst)] =
            if (first.i64MulTransform == id)
              second.i64MulTransform
            else if (second.i64MulTransform == id)
              first.i64MulTransform
            else { (ctx: Ctx, t: i64.Mul.type) =>
              first.i64MulTransform(ctx, t).flatMap {
                case (ctx, t: i64.Mul.type) => second.i64MulTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64DivSTransform: (Ctx, i64.DivS.type) => F[(Ctx, Inst)] =
            if (first.i64DivSTransform == id)
              second.i64DivSTransform
            else if (second.i64DivSTransform == id)
              first.i64DivSTransform
            else { (ctx: Ctx, t: i64.DivS.type) =>
              first.i64DivSTransform(ctx, t).flatMap {
                case (ctx, t: i64.DivS.type) => second.i64DivSTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64DivUTransform: (Ctx, i64.DivU.type) => F[(Ctx, Inst)] =
            if (first.i64DivUTransform == id)
              second.i64DivUTransform
            else if (second.i64DivUTransform == id)
              first.i64DivUTransform
            else { (ctx: Ctx, t: i64.DivU.type) =>
              first.i64DivUTransform(ctx, t).flatMap {
                case (ctx, t: i64.DivU.type) => second.i64DivUTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64RemSTransform: (Ctx, i64.RemS.type) => F[(Ctx, Inst)] =
            if (first.i64RemSTransform == id)
              second.i64RemSTransform
            else if (second.i64RemSTransform == id)
              first.i64RemSTransform
            else { (ctx: Ctx, t: i64.RemS.type) =>
              first.i64RemSTransform(ctx, t).flatMap {
                case (ctx, t: i64.RemS.type) => second.i64RemSTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64RemUTransform: (Ctx, i64.RemU.type) => F[(Ctx, Inst)] =
            if (first.i64RemUTransform == id)
              second.i64RemUTransform
            else if (second.i64RemUTransform == id)
              first.i64RemUTransform
            else { (ctx: Ctx, t: i64.RemU.type) =>
              first.i64RemUTransform(ctx, t).flatMap {
                case (ctx, t: i64.RemU.type) => second.i64RemUTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64AndTransform: (Ctx, i64.And.type) => F[(Ctx, Inst)] =
            if (first.i64AndTransform == id)
              second.i64AndTransform
            else if (second.i64AndTransform == id)
              first.i64AndTransform
            else { (ctx: Ctx, t: i64.And.type) =>
              first.i64AndTransform(ctx, t).flatMap {
                case (ctx, t: i64.And.type) => second.i64AndTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64OrTransform: (Ctx, i64.Or.type) => F[(Ctx, Inst)] =
            if (first.i64OrTransform == id)
              second.i64OrTransform
            else if (second.i64OrTransform == id)
              first.i64OrTransform
            else { (ctx: Ctx, t: i64.Or.type) =>
              first.i64OrTransform(ctx, t).flatMap {
                case (ctx, t: i64.Or.type) => second.i64OrTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64XorTransform: (Ctx, i64.Xor.type) => F[(Ctx, Inst)] =
            if (first.i64XorTransform == id)
              second.i64XorTransform
            else if (second.i64XorTransform == id)
              first.i64XorTransform
            else { (ctx: Ctx, t: i64.Xor.type) =>
              first.i64XorTransform(ctx, t).flatMap {
                case (ctx, t: i64.Xor.type) => second.i64XorTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64ShlTransform: (Ctx, i64.Shl.type) => F[(Ctx, Inst)] =
            if (first.i64ShlTransform == id)
              second.i64ShlTransform
            else if (second.i64ShlTransform == id)
              first.i64ShlTransform
            else { (ctx: Ctx, t: i64.Shl.type) =>
              first.i64ShlTransform(ctx, t).flatMap {
                case (ctx, t: i64.Shl.type) => second.i64ShlTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64ShrSTransform: (Ctx, i64.ShrS.type) => F[(Ctx, Inst)] =
            if (first.i64ShrSTransform == id)
              second.i64ShrSTransform
            else if (second.i64ShrSTransform == id)
              first.i64ShrSTransform
            else { (ctx: Ctx, t: i64.ShrS.type) =>
              first.i64ShrSTransform(ctx, t).flatMap {
                case (ctx, t: i64.ShrS.type) => second.i64ShrSTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64ShrUTransform: (Ctx, i64.ShrU.type) => F[(Ctx, Inst)] =
            if (first.i64ShrUTransform == id)
              second.i64ShrUTransform
            else if (second.i64ShrUTransform == id)
              first.i64ShrUTransform
            else { (ctx: Ctx, t: i64.ShrU.type) =>
              first.i64ShrUTransform(ctx, t).flatMap {
                case (ctx, t: i64.ShrU.type) => second.i64ShrUTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64RotlTransform: (Ctx, i64.Rotl.type) => F[(Ctx, Inst)] =
            if (first.i64RotlTransform == id)
              second.i64RotlTransform
            else if (second.i64RotlTransform == id)
              first.i64RotlTransform
            else { (ctx: Ctx, t: i64.Rotl.type) =>
              first.i64RotlTransform(ctx, t).flatMap {
                case (ctx, t: i64.Rotl.type) => second.i64RotlTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64RotrTransform: (Ctx, i64.Rotr.type) => F[(Ctx, Inst)] =
            if (first.i64RotrTransform == id)
              second.i64RotrTransform
            else if (second.i64RotrTransform == id)
              first.i64RotrTransform
            else { (ctx: Ctx, t: i64.Rotr.type) =>
              first.i64RotrTransform(ctx, t).flatMap {
                case (ctx, t: i64.Rotr.type) => second.i64RotrTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val i64EqzTransform: (Ctx, i64.Eqz.type) => F[(Ctx, Inst)] =
            if (first.i64EqzTransform == id)
              second.i64EqzTransform
            else if (second.i64EqzTransform == id)
              first.i64EqzTransform
            else { (ctx: Ctx, t: i64.Eqz.type) =>
              first.i64EqzTransform(ctx, t).flatMap {
                case (ctx, t: i64.Eqz.type) => second.i64EqzTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64EqTransform: (Ctx, i64.Eq.type) => F[(Ctx, Inst)] =
            if (first.i64EqTransform == id)
              second.i64EqTransform
            else if (second.i64EqTransform == id)
              first.i64EqTransform
            else { (ctx: Ctx, t: i64.Eq.type) =>
              first.i64EqTransform(ctx, t).flatMap {
                case (ctx, t: i64.Eq.type) => second.i64EqTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64NeTransform: (Ctx, i64.Ne.type) => F[(Ctx, Inst)] =
            if (first.i64NeTransform == id)
              second.i64NeTransform
            else if (second.i64NeTransform == id)
              first.i64NeTransform
            else { (ctx: Ctx, t: i64.Ne.type) =>
              first.i64NeTransform(ctx, t).flatMap {
                case (ctx, t: i64.Ne.type) => second.i64NeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64LtSTransform: (Ctx, i64.LtS.type) => F[(Ctx, Inst)] =
            if (first.i64LtSTransform == id)
              second.i64LtSTransform
            else if (second.i64LtSTransform == id)
              first.i64LtSTransform
            else { (ctx: Ctx, t: i64.LtS.type) =>
              first.i64LtSTransform(ctx, t).flatMap {
                case (ctx, t: i64.LtS.type) => second.i64LtSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64LtUTransform: (Ctx, i64.LtU.type) => F[(Ctx, Inst)] =
            if (first.i64LtUTransform == id)
              second.i64LtUTransform
            else if (second.i64LtUTransform == id)
              first.i64LtUTransform
            else { (ctx: Ctx, t: i64.LtU.type) =>
              first.i64LtUTransform(ctx, t).flatMap {
                case (ctx, t: i64.LtU.type) => second.i64LtUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64GtSTransform: (Ctx, i64.GtS.type) => F[(Ctx, Inst)] =
            if (first.i64GtSTransform == id)
              second.i64GtSTransform
            else if (second.i64GtSTransform == id)
              first.i64GtSTransform
            else { (ctx: Ctx, t: i64.GtS.type) =>
              first.i64GtSTransform(ctx, t).flatMap {
                case (ctx, t: i64.GtS.type) => second.i64GtSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64GtUTransform: (Ctx, i64.GtU.type) => F[(Ctx, Inst)] =
            if (first.i64GtUTransform == id)
              second.i64GtUTransform
            else if (second.i64GtUTransform == id)
              first.i64GtUTransform
            else { (ctx: Ctx, t: i64.GtU.type) =>
              first.i64GtUTransform(ctx, t).flatMap {
                case (ctx, t: i64.GtU.type) => second.i64GtUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64LeSTransform: (Ctx, i64.LeS.type) => F[(Ctx, Inst)] =
            if (first.i64LeSTransform == id)
              second.i64LeSTransform
            else if (second.i64LeSTransform == id)
              first.i64LeSTransform
            else { (ctx: Ctx, t: i64.LeS.type) =>
              first.i64LeSTransform(ctx, t).flatMap {
                case (ctx, t: i64.LeS.type) => second.i64LeSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64LeUTransform: (Ctx, i64.LeU.type) => F[(Ctx, Inst)] =
            if (first.i64LeUTransform == id)
              second.i64LeUTransform
            else if (second.i64LeUTransform == id)
              first.i64LeUTransform
            else { (ctx: Ctx, t: i64.LeU.type) =>
              first.i64LeUTransform(ctx, t).flatMap {
                case (ctx, t: i64.LeU.type) => second.i64LeUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64GeSTransform: (Ctx, i64.GeS.type) => F[(Ctx, Inst)] =
            if (first.i64GeSTransform == id)
              second.i64GeSTransform
            else if (second.i64GeSTransform == id)
              first.i64GeSTransform
            else { (ctx: Ctx, t: i64.GeS.type) =>
              first.i64GeSTransform(ctx, t).flatMap {
                case (ctx, t: i64.GeS.type) => second.i64GeSTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64GeUTransform: (Ctx, i64.GeU.type) => F[(Ctx, Inst)] =
            if (first.i64GeUTransform == id)
              second.i64GeUTransform
            else if (second.i64GeUTransform == id)
              first.i64GeUTransform
            else { (ctx: Ctx, t: i64.GeU.type) =>
              first.i64GeUTransform(ctx, t).flatMap {
                case (ctx, t: i64.GeU.type) => second.i64GeUTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val i64ExtendSI32Transform: (Ctx, i64.ExtendSI32.type) => F[(Ctx, Inst)] =
            if (first.i64ExtendSI32Transform == id)
              second.i64ExtendSI32Transform
            else if (second.i64ExtendSI32Transform == id)
              first.i64ExtendSI32Transform
            else { (ctx: Ctx, t: i64.ExtendSI32.type) =>
              first.i64ExtendSI32Transform(ctx, t).flatMap {
                case (ctx, t: i64.ExtendSI32.type) => second.i64ExtendSI32Transform(ctx, t)
                case (ctx, t)                      => second.transform(ctx, t)
              }
            }
          override val i64ExtendUI32Transform: (Ctx, i64.ExtendUI32.type) => F[(Ctx, Inst)] =
            if (first.i64ExtendUI32Transform == id)
              second.i64ExtendUI32Transform
            else if (second.i64ExtendUI32Transform == id)
              first.i64ExtendUI32Transform
            else { (ctx: Ctx, t: i64.ExtendUI32.type) =>
              first.i64ExtendUI32Transform(ctx, t).flatMap {
                case (ctx, t: i64.ExtendUI32.type) => second.i64ExtendUI32Transform(ctx, t)
                case (ctx, t)                      => second.transform(ctx, t)
              }
            }
          override val i64TruncSF32Transform: (Ctx, i64.TruncSF32.type) => F[(Ctx, Inst)] =
            if (first.i64TruncSF32Transform == id)
              second.i64TruncSF32Transform
            else if (second.i64TruncSF32Transform == id)
              first.i64TruncSF32Transform
            else { (ctx: Ctx, t: i64.TruncSF32.type) =>
              first.i64TruncSF32Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncSF32.type) => second.i64TruncSF32Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i64TruncUF32Transform: (Ctx, i64.TruncUF32.type) => F[(Ctx, Inst)] =
            if (first.i64TruncUF32Transform == id)
              second.i64TruncUF32Transform
            else if (second.i64TruncUF32Transform == id)
              first.i64TruncUF32Transform
            else { (ctx: Ctx, t: i64.TruncUF32.type) =>
              first.i64TruncUF32Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncUF32.type) => second.i64TruncUF32Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i64TruncSF64Transform: (Ctx, i64.TruncSF64.type) => F[(Ctx, Inst)] =
            if (first.i64TruncSF64Transform == id)
              second.i64TruncSF64Transform
            else if (second.i64TruncSF64Transform == id)
              first.i64TruncSF64Transform
            else { (ctx: Ctx, t: i64.TruncSF64.type) =>
              first.i64TruncSF64Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncSF64.type) => second.i64TruncSF64Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i64TruncUF64Transform: (Ctx, i64.TruncUF64.type) => F[(Ctx, Inst)] =
            if (first.i64TruncUF64Transform == id)
              second.i64TruncUF64Transform
            else if (second.i64TruncUF64Transform == id)
              first.i64TruncUF64Transform
            else { (ctx: Ctx, t: i64.TruncUF64.type) =>
              first.i64TruncUF64Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncUF64.type) => second.i64TruncUF64Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val i64TruncSatSF32Transform: (Ctx, i64.TruncSatSF32.type) => F[(Ctx, Inst)] =
            if (first.i64TruncSatSF32Transform == id)
              second.i64TruncSatSF32Transform
            else if (second.i64TruncSatSF32Transform == id)
              first.i64TruncSatSF32Transform
            else { (ctx: Ctx, t: i64.TruncSatSF32.type) =>
              first.i64TruncSatSF32Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncSatSF32.type) => second.i64TruncSatSF32Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i64TruncSatUF32Transform: (Ctx, i64.TruncSatUF32.type) => F[(Ctx, Inst)] =
            if (first.i64TruncSatUF32Transform == id)
              second.i64TruncSatUF32Transform
            else if (second.i64TruncSatUF32Transform == id)
              first.i64TruncSatUF32Transform
            else { (ctx: Ctx, t: i64.TruncSatUF32.type) =>
              first.i64TruncSatUF32Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncSatUF32.type) => second.i64TruncSatUF32Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i64TruncSatSF64Transform: (Ctx, i64.TruncSatSF64.type) => F[(Ctx, Inst)] =
            if (first.i64TruncSatSF64Transform == id)
              second.i64TruncSatSF64Transform
            else if (second.i64TruncSatSF64Transform == id)
              first.i64TruncSatSF64Transform
            else { (ctx: Ctx, t: i64.TruncSatSF64.type) =>
              first.i64TruncSatSF64Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncSatSF64.type) => second.i64TruncSatSF64Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i64TruncSatUF64Transform: (Ctx, i64.TruncSatUF64.type) => F[(Ctx, Inst)] =
            if (first.i64TruncSatUF64Transform == id)
              second.i64TruncSatUF64Transform
            else if (second.i64TruncSatUF64Transform == id)
              first.i64TruncSatUF64Transform
            else { (ctx: Ctx, t: i64.TruncSatUF64.type) =>
              first.i64TruncSatUF64Transform(ctx, t).flatMap {
                case (ctx, t: i64.TruncSatUF64.type) => second.i64TruncSatUF64Transform(ctx, t)
                case (ctx, t)                        => second.transform(ctx, t)
              }
            }
          override val i64ReinterpretF64Transform: (Ctx, i64.ReinterpretF64.type) => F[(Ctx, Inst)] =
            if (first.i64ReinterpretF64Transform == id)
              second.i64ReinterpretF64Transform
            else if (second.i64ReinterpretF64Transform == id)
              first.i64ReinterpretF64Transform
            else { (ctx: Ctx, t: i64.ReinterpretF64.type) =>
              first.i64ReinterpretF64Transform(ctx, t).flatMap {
                case (ctx, t: i64.ReinterpretF64.type) => second.i64ReinterpretF64Transform(ctx, t)
                case (ctx, t)                          => second.transform(ctx, t)
              }
            }
          override val i64LoadTransform: (Ctx, i64.Load) => F[(Ctx, Inst)] =
            if (first.i64LoadTransform == id)
              second.i64LoadTransform
            else if (second.i64LoadTransform == id)
              first.i64LoadTransform
            else { (ctx: Ctx, t: i64.Load) =>
              first.i64LoadTransform(ctx, t).flatMap {
                case (ctx, t: i64.Load) => second.i64LoadTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val i64StoreTransform: (Ctx, i64.Store) => F[(Ctx, Inst)] =
            if (first.i64StoreTransform == id)
              second.i64StoreTransform
            else if (second.i64StoreTransform == id)
              first.i64StoreTransform
            else { (ctx: Ctx, t: i64.Store) =>
              first.i64StoreTransform(ctx, t).flatMap {
                case (ctx, t: i64.Store) => second.i64StoreTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val i64Load8STransform: (Ctx, i64.Load8S) => F[(Ctx, Inst)] =
            if (first.i64Load8STransform == id)
              second.i64Load8STransform
            else if (second.i64Load8STransform == id)
              first.i64Load8STransform
            else { (ctx: Ctx, t: i64.Load8S) =>
              first.i64Load8STransform(ctx, t).flatMap {
                case (ctx, t: i64.Load8S) => second.i64Load8STransform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val i64Load8UTransform: (Ctx, i64.Load8U) => F[(Ctx, Inst)] =
            if (first.i64Load8UTransform == id)
              second.i64Load8UTransform
            else if (second.i64Load8UTransform == id)
              first.i64Load8UTransform
            else { (ctx: Ctx, t: i64.Load8U) =>
              first.i64Load8UTransform(ctx, t).flatMap {
                case (ctx, t: i64.Load8U) => second.i64Load8UTransform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val i64Load16STransform: (Ctx, i64.Load16S) => F[(Ctx, Inst)] =
            if (first.i64Load16STransform == id)
              second.i64Load16STransform
            else if (second.i64Load16STransform == id)
              first.i64Load16STransform
            else { (ctx: Ctx, t: i64.Load16S) =>
              first.i64Load16STransform(ctx, t).flatMap {
                case (ctx, t: i64.Load16S) => second.i64Load16STransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64Load16UTransform: (Ctx, i64.Load16U) => F[(Ctx, Inst)] =
            if (first.i64Load16UTransform == id)
              second.i64Load16UTransform
            else if (second.i64Load16UTransform == id)
              first.i64Load16UTransform
            else { (ctx: Ctx, t: i64.Load16U) =>
              first.i64Load16UTransform(ctx, t).flatMap {
                case (ctx, t: i64.Load16U) => second.i64Load16UTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64Load32STransform: (Ctx, i64.Load32S) => F[(Ctx, Inst)] =
            if (first.i64Load32STransform == id)
              second.i64Load32STransform
            else if (second.i64Load32STransform == id)
              first.i64Load32STransform
            else { (ctx: Ctx, t: i64.Load32S) =>
              first.i64Load32STransform(ctx, t).flatMap {
                case (ctx, t: i64.Load32S) => second.i64Load32STransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64Load32UTransform: (Ctx, i64.Load32U) => F[(Ctx, Inst)] =
            if (first.i64Load32UTransform == id)
              second.i64Load32UTransform
            else if (second.i64Load32UTransform == id)
              first.i64Load32UTransform
            else { (ctx: Ctx, t: i64.Load32U) =>
              first.i64Load32UTransform(ctx, t).flatMap {
                case (ctx, t: i64.Load32U) => second.i64Load32UTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64Store8Transform: (Ctx, i64.Store8) => F[(Ctx, Inst)] =
            if (first.i64Store8Transform == id)
              second.i64Store8Transform
            else if (second.i64Store8Transform == id)
              first.i64Store8Transform
            else { (ctx: Ctx, t: i64.Store8) =>
              first.i64Store8Transform(ctx, t).flatMap {
                case (ctx, t: i64.Store8) => second.i64Store8Transform(ctx, t)
                case (ctx, t)             => second.transform(ctx, t)
              }
            }
          override val i64Store16Transform: (Ctx, i64.Store16) => F[(Ctx, Inst)] =
            if (first.i64Store16Transform == id)
              second.i64Store16Transform
            else if (second.i64Store16Transform == id)
              first.i64Store16Transform
            else { (ctx: Ctx, t: i64.Store16) =>
              first.i64Store16Transform(ctx, t).flatMap {
                case (ctx, t: i64.Store16) => second.i64Store16Transform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val i64Store32Transform: (Ctx, i64.Store32) => F[(Ctx, Inst)] =
            if (first.i64Store32Transform == id)
              second.i64Store32Transform
            else if (second.i64Store32Transform == id)
              first.i64Store32Transform
            else { (ctx: Ctx, t: i64.Store32) =>
              first.i64Store32Transform(ctx, t).flatMap {
                case (ctx, t: i64.Store32) => second.i64Store32Transform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f32ConstTransform: (Ctx, f32.Const) => F[(Ctx, Inst)] =
            if (first.f32ConstTransform == id)
              second.f32ConstTransform
            else if (second.f32ConstTransform == id)
              first.f32ConstTransform
            else { (ctx: Ctx, t: f32.Const) =>
              first.f32ConstTransform(ctx, t).flatMap {
                case (ctx, t: f32.Const) => second.f32ConstTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val f32AbsTransform: (Ctx, f32.Abs.type) => F[(Ctx, Inst)] =
            if (first.f32AbsTransform == id)
              second.f32AbsTransform
            else if (second.f32AbsTransform == id)
              first.f32AbsTransform
            else { (ctx: Ctx, t: f32.Abs.type) =>
              first.f32AbsTransform(ctx, t).flatMap {
                case (ctx, t: f32.Abs.type) => second.f32AbsTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32NegTransform: (Ctx, f32.Neg.type) => F[(Ctx, Inst)] =
            if (first.f32NegTransform == id)
              second.f32NegTransform
            else if (second.f32NegTransform == id)
              first.f32NegTransform
            else { (ctx: Ctx, t: f32.Neg.type) =>
              first.f32NegTransform(ctx, t).flatMap {
                case (ctx, t: f32.Neg.type) => second.f32NegTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32SqrtTransform: (Ctx, f32.Sqrt.type) => F[(Ctx, Inst)] =
            if (first.f32SqrtTransform == id)
              second.f32SqrtTransform
            else if (second.f32SqrtTransform == id)
              first.f32SqrtTransform
            else { (ctx: Ctx, t: f32.Sqrt.type) =>
              first.f32SqrtTransform(ctx, t).flatMap {
                case (ctx, t: f32.Sqrt.type) => second.f32SqrtTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val f32CeilTransform: (Ctx, f32.Ceil.type) => F[(Ctx, Inst)] =
            if (first.f32CeilTransform == id)
              second.f32CeilTransform
            else if (second.f32CeilTransform == id)
              first.f32CeilTransform
            else { (ctx: Ctx, t: f32.Ceil.type) =>
              first.f32CeilTransform(ctx, t).flatMap {
                case (ctx, t: f32.Ceil.type) => second.f32CeilTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val f32FloorTransform: (Ctx, f32.Floor.type) => F[(Ctx, Inst)] =
            if (first.f32FloorTransform == id)
              second.f32FloorTransform
            else if (second.f32FloorTransform == id)
              first.f32FloorTransform
            else { (ctx: Ctx, t: f32.Floor.type) =>
              first.f32FloorTransform(ctx, t).flatMap {
                case (ctx, t: f32.Floor.type) => second.f32FloorTransform(ctx, t)
                case (ctx, t)                 => second.transform(ctx, t)
              }
            }
          override val f32TruncTransform: (Ctx, f32.Trunc.type) => F[(Ctx, Inst)] =
            if (first.f32TruncTransform == id)
              second.f32TruncTransform
            else if (second.f32TruncTransform == id)
              first.f32TruncTransform
            else { (ctx: Ctx, t: f32.Trunc.type) =>
              first.f32TruncTransform(ctx, t).flatMap {
                case (ctx, t: f32.Trunc.type) => second.f32TruncTransform(ctx, t)
                case (ctx, t)                 => second.transform(ctx, t)
              }
            }
          override val f32NearestTransform: (Ctx, f32.Nearest.type) => F[(Ctx, Inst)] =
            if (first.f32NearestTransform == id)
              second.f32NearestTransform
            else if (second.f32NearestTransform == id)
              first.f32NearestTransform
            else { (ctx: Ctx, t: f32.Nearest.type) =>
              first.f32NearestTransform(ctx, t).flatMap {
                case (ctx, t: f32.Nearest.type) => second.f32NearestTransform(ctx, t)
                case (ctx, t)                   => second.transform(ctx, t)
              }
            }
          override val f32AddTransform: (Ctx, f32.Add.type) => F[(Ctx, Inst)] =
            if (first.f32AddTransform == id)
              second.f32AddTransform
            else if (second.f32AddTransform == id)
              first.f32AddTransform
            else { (ctx: Ctx, t: f32.Add.type) =>
              first.f32AddTransform(ctx, t).flatMap {
                case (ctx, t: f32.Add.type) => second.f32AddTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32SubTransform: (Ctx, f32.Sub.type) => F[(Ctx, Inst)] =
            if (first.f32SubTransform == id)
              second.f32SubTransform
            else if (second.f32SubTransform == id)
              first.f32SubTransform
            else { (ctx: Ctx, t: f32.Sub.type) =>
              first.f32SubTransform(ctx, t).flatMap {
                case (ctx, t: f32.Sub.type) => second.f32SubTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32MulTransform: (Ctx, f32.Mul.type) => F[(Ctx, Inst)] =
            if (first.f32MulTransform == id)
              second.f32MulTransform
            else if (second.f32MulTransform == id)
              first.f32MulTransform
            else { (ctx: Ctx, t: f32.Mul.type) =>
              first.f32MulTransform(ctx, t).flatMap {
                case (ctx, t: f32.Mul.type) => second.f32MulTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32DivTransform: (Ctx, f32.Div.type) => F[(Ctx, Inst)] =
            if (first.f32DivTransform == id)
              second.f32DivTransform
            else if (second.f32DivTransform == id)
              first.f32DivTransform
            else { (ctx: Ctx, t: f32.Div.type) =>
              first.f32DivTransform(ctx, t).flatMap {
                case (ctx, t: f32.Div.type) => second.f32DivTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32MinTransform: (Ctx, f32.Min.type) => F[(Ctx, Inst)] =
            if (first.f32MinTransform == id)
              second.f32MinTransform
            else if (second.f32MinTransform == id)
              first.f32MinTransform
            else { (ctx: Ctx, t: f32.Min.type) =>
              first.f32MinTransform(ctx, t).flatMap {
                case (ctx, t: f32.Min.type) => second.f32MinTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32MaxTransform: (Ctx, f32.Max.type) => F[(Ctx, Inst)] =
            if (first.f32MaxTransform == id)
              second.f32MaxTransform
            else if (second.f32MaxTransform == id)
              first.f32MaxTransform
            else { (ctx: Ctx, t: f32.Max.type) =>
              first.f32MaxTransform(ctx, t).flatMap {
                case (ctx, t: f32.Max.type) => second.f32MaxTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f32CopysignTransform: (Ctx, f32.Copysign.type) => F[(Ctx, Inst)] =
            if (first.f32CopysignTransform == id)
              second.f32CopysignTransform
            else if (second.f32CopysignTransform == id)
              first.f32CopysignTransform
            else { (ctx: Ctx, t: f32.Copysign.type) =>
              first.f32CopysignTransform(ctx, t).flatMap {
                case (ctx, t: f32.Copysign.type) => second.f32CopysignTransform(ctx, t)
                case (ctx, t)                    => second.transform(ctx, t)
              }
            }
          override val f32EqTransform: (Ctx, f32.Eq.type) => F[(Ctx, Inst)] =
            if (first.f32EqTransform == id)
              second.f32EqTransform
            else if (second.f32EqTransform == id)
              first.f32EqTransform
            else { (ctx: Ctx, t: f32.Eq.type) =>
              first.f32EqTransform(ctx, t).flatMap {
                case (ctx, t: f32.Eq.type) => second.f32EqTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f32NeTransform: (Ctx, f32.Ne.type) => F[(Ctx, Inst)] =
            if (first.f32NeTransform == id)
              second.f32NeTransform
            else if (second.f32NeTransform == id)
              first.f32NeTransform
            else { (ctx: Ctx, t: f32.Ne.type) =>
              first.f32NeTransform(ctx, t).flatMap {
                case (ctx, t: f32.Ne.type) => second.f32NeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f32LtTransform: (Ctx, f32.Lt.type) => F[(Ctx, Inst)] =
            if (first.f32LtTransform == id)
              second.f32LtTransform
            else if (second.f32LtTransform == id)
              first.f32LtTransform
            else { (ctx: Ctx, t: f32.Lt.type) =>
              first.f32LtTransform(ctx, t).flatMap {
                case (ctx, t: f32.Lt.type) => second.f32LtTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f32GtTransform: (Ctx, f32.Gt.type) => F[(Ctx, Inst)] =
            if (first.f32GtTransform == id)
              second.f32GtTransform
            else if (second.f32GtTransform == id)
              first.f32GtTransform
            else { (ctx: Ctx, t: f32.Gt.type) =>
              first.f32GtTransform(ctx, t).flatMap {
                case (ctx, t: f32.Gt.type) => second.f32GtTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f32LeTransform: (Ctx, f32.Le.type) => F[(Ctx, Inst)] =
            if (first.f32LeTransform == id)
              second.f32LeTransform
            else if (second.f32LeTransform == id)
              first.f32LeTransform
            else { (ctx: Ctx, t: f32.Le.type) =>
              first.f32LeTransform(ctx, t).flatMap {
                case (ctx, t: f32.Le.type) => second.f32LeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f32GeTransform: (Ctx, f32.Ge.type) => F[(Ctx, Inst)] =
            if (first.f32GeTransform == id)
              second.f32GeTransform
            else if (second.f32GeTransform == id)
              first.f32GeTransform
            else { (ctx: Ctx, t: f32.Ge.type) =>
              first.f32GeTransform(ctx, t).flatMap {
                case (ctx, t: f32.Ge.type) => second.f32GeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f32DemoteF64Transform: (Ctx, f32.DemoteF64.type) => F[(Ctx, Inst)] =
            if (first.f32DemoteF64Transform == id)
              second.f32DemoteF64Transform
            else if (second.f32DemoteF64Transform == id)
              first.f32DemoteF64Transform
            else { (ctx: Ctx, t: f32.DemoteF64.type) =>
              first.f32DemoteF64Transform(ctx, t).flatMap {
                case (ctx, t: f32.DemoteF64.type) => second.f32DemoteF64Transform(ctx, t)
                case (ctx, t)                     => second.transform(ctx, t)
              }
            }
          override val f32ConvertSI32Transform: (Ctx, f32.ConvertSI32.type) => F[(Ctx, Inst)] =
            if (first.f32ConvertSI32Transform == id)
              second.f32ConvertSI32Transform
            else if (second.f32ConvertSI32Transform == id)
              first.f32ConvertSI32Transform
            else { (ctx: Ctx, t: f32.ConvertSI32.type) =>
              first.f32ConvertSI32Transform(ctx, t).flatMap {
                case (ctx, t: f32.ConvertSI32.type) => second.f32ConvertSI32Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f32ConvertUI32Transform: (Ctx, f32.ConvertUI32.type) => F[(Ctx, Inst)] =
            if (first.f32ConvertUI32Transform == id)
              second.f32ConvertUI32Transform
            else if (second.f32ConvertUI32Transform == id)
              first.f32ConvertUI32Transform
            else { (ctx: Ctx, t: f32.ConvertUI32.type) =>
              first.f32ConvertUI32Transform(ctx, t).flatMap {
                case (ctx, t: f32.ConvertUI32.type) => second.f32ConvertUI32Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f32ConvertSI64Transform: (Ctx, f32.ConvertSI64.type) => F[(Ctx, Inst)] =
            if (first.f32ConvertSI64Transform == id)
              second.f32ConvertSI64Transform
            else if (second.f32ConvertSI64Transform == id)
              first.f32ConvertSI64Transform
            else { (ctx: Ctx, t: f32.ConvertSI64.type) =>
              first.f32ConvertSI64Transform(ctx, t).flatMap {
                case (ctx, t: f32.ConvertSI64.type) => second.f32ConvertSI64Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f32ConvertUI64Transform: (Ctx, f32.ConvertUI64.type) => F[(Ctx, Inst)] =
            if (first.f32ConvertUI64Transform == id)
              second.f32ConvertUI64Transform
            else if (second.f32ConvertUI64Transform == id)
              first.f32ConvertUI64Transform
            else { (ctx: Ctx, t: f32.ConvertUI64.type) =>
              first.f32ConvertUI64Transform(ctx, t).flatMap {
                case (ctx, t: f32.ConvertUI64.type) => second.f32ConvertUI64Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f32ReinterpretI32Transform: (Ctx, f32.ReinterpretI32.type) => F[(Ctx, Inst)] =
            if (first.f32ReinterpretI32Transform == id)
              second.f32ReinterpretI32Transform
            else if (second.f32ReinterpretI32Transform == id)
              first.f32ReinterpretI32Transform
            else { (ctx: Ctx, t: f32.ReinterpretI32.type) =>
              first.f32ReinterpretI32Transform(ctx, t).flatMap {
                case (ctx, t: f32.ReinterpretI32.type) => second.f32ReinterpretI32Transform(ctx, t)
                case (ctx, t)                          => second.transform(ctx, t)
              }
            }
          override val f32LoadTransform: (Ctx, f32.Load) => F[(Ctx, Inst)] =
            if (first.f32LoadTransform == id)
              second.f32LoadTransform
            else if (second.f32LoadTransform == id)
              first.f32LoadTransform
            else { (ctx: Ctx, t: f32.Load) =>
              first.f32LoadTransform(ctx, t).flatMap {
                case (ctx, t: f32.Load) => second.f32LoadTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val f32StoreTransform: (Ctx, f32.Store) => F[(Ctx, Inst)] =
            if (first.f32StoreTransform == id)
              second.f32StoreTransform
            else if (second.f32StoreTransform == id)
              first.f32StoreTransform
            else { (ctx: Ctx, t: f32.Store) =>
              first.f32StoreTransform(ctx, t).flatMap {
                case (ctx, t: f32.Store) => second.f32StoreTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val f64ConstTransform: (Ctx, f64.Const) => F[(Ctx, Inst)] =
            if (first.f64ConstTransform == id)
              second.f64ConstTransform
            else if (second.f64ConstTransform == id)
              first.f64ConstTransform
            else { (ctx: Ctx, t: f64.Const) =>
              first.f64ConstTransform(ctx, t).flatMap {
                case (ctx, t: f64.Const) => second.f64ConstTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val f64AbsTransform: (Ctx, f64.Abs.type) => F[(Ctx, Inst)] =
            if (first.f64AbsTransform == id)
              second.f64AbsTransform
            else if (second.f64AbsTransform == id)
              first.f64AbsTransform
            else { (ctx: Ctx, t: f64.Abs.type) =>
              first.f64AbsTransform(ctx, t).flatMap {
                case (ctx, t: f64.Abs.type) => second.f64AbsTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64NegTransform: (Ctx, f64.Neg.type) => F[(Ctx, Inst)] =
            if (first.f64NegTransform == id)
              second.f64NegTransform
            else if (second.f64NegTransform == id)
              first.f64NegTransform
            else { (ctx: Ctx, t: f64.Neg.type) =>
              first.f64NegTransform(ctx, t).flatMap {
                case (ctx, t: f64.Neg.type) => second.f64NegTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64SqrtTransform: (Ctx, f64.Sqrt.type) => F[(Ctx, Inst)] =
            if (first.f64SqrtTransform == id)
              second.f64SqrtTransform
            else if (second.f64SqrtTransform == id)
              first.f64SqrtTransform
            else { (ctx: Ctx, t: f64.Sqrt.type) =>
              first.f64SqrtTransform(ctx, t).flatMap {
                case (ctx, t: f64.Sqrt.type) => second.f64SqrtTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val f64CeilTransform: (Ctx, f64.Ceil.type) => F[(Ctx, Inst)] =
            if (first.f64CeilTransform == id)
              second.f64CeilTransform
            else if (second.f64CeilTransform == id)
              first.f64CeilTransform
            else { (ctx: Ctx, t: f64.Ceil.type) =>
              first.f64CeilTransform(ctx, t).flatMap {
                case (ctx, t: f64.Ceil.type) => second.f64CeilTransform(ctx, t)
                case (ctx, t)                => second.transform(ctx, t)
              }
            }
          override val f64FloorTransform: (Ctx, f64.Floor.type) => F[(Ctx, Inst)] =
            if (first.f64FloorTransform == id)
              second.f64FloorTransform
            else if (second.f64FloorTransform == id)
              first.f64FloorTransform
            else { (ctx: Ctx, t: f64.Floor.type) =>
              first.f64FloorTransform(ctx, t).flatMap {
                case (ctx, t: f64.Floor.type) => second.f64FloorTransform(ctx, t)
                case (ctx, t)                 => second.transform(ctx, t)
              }
            }
          override val f64TruncTransform: (Ctx, f64.Trunc.type) => F[(Ctx, Inst)] =
            if (first.f64TruncTransform == id)
              second.f64TruncTransform
            else if (second.f64TruncTransform == id)
              first.f64TruncTransform
            else { (ctx: Ctx, t: f64.Trunc.type) =>
              first.f64TruncTransform(ctx, t).flatMap {
                case (ctx, t: f64.Trunc.type) => second.f64TruncTransform(ctx, t)
                case (ctx, t)                 => second.transform(ctx, t)
              }
            }
          override val f64NearestTransform: (Ctx, f64.Nearest.type) => F[(Ctx, Inst)] =
            if (first.f64NearestTransform == id)
              second.f64NearestTransform
            else if (second.f64NearestTransform == id)
              first.f64NearestTransform
            else { (ctx: Ctx, t: f64.Nearest.type) =>
              first.f64NearestTransform(ctx, t).flatMap {
                case (ctx, t: f64.Nearest.type) => second.f64NearestTransform(ctx, t)
                case (ctx, t)                   => second.transform(ctx, t)
              }
            }
          override val f64AddTransform: (Ctx, f64.Add.type) => F[(Ctx, Inst)] =
            if (first.f64AddTransform == id)
              second.f64AddTransform
            else if (second.f64AddTransform == id)
              first.f64AddTransform
            else { (ctx: Ctx, t: f64.Add.type) =>
              first.f64AddTransform(ctx, t).flatMap {
                case (ctx, t: f64.Add.type) => second.f64AddTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64SubTransform: (Ctx, f64.Sub.type) => F[(Ctx, Inst)] =
            if (first.f64SubTransform == id)
              second.f64SubTransform
            else if (second.f64SubTransform == id)
              first.f64SubTransform
            else { (ctx: Ctx, t: f64.Sub.type) =>
              first.f64SubTransform(ctx, t).flatMap {
                case (ctx, t: f64.Sub.type) => second.f64SubTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64MulTransform: (Ctx, f64.Mul.type) => F[(Ctx, Inst)] =
            if (first.f64MulTransform == id)
              second.f64MulTransform
            else if (second.f64MulTransform == id)
              first.f64MulTransform
            else { (ctx: Ctx, t: f64.Mul.type) =>
              first.f64MulTransform(ctx, t).flatMap {
                case (ctx, t: f64.Mul.type) => second.f64MulTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64DivTransform: (Ctx, f64.Div.type) => F[(Ctx, Inst)] =
            if (first.f64DivTransform == id)
              second.f64DivTransform
            else if (second.f64DivTransform == id)
              first.f64DivTransform
            else { (ctx: Ctx, t: f64.Div.type) =>
              first.f64DivTransform(ctx, t).flatMap {
                case (ctx, t: f64.Div.type) => second.f64DivTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64MinTransform: (Ctx, f64.Min.type) => F[(Ctx, Inst)] =
            if (first.f64MinTransform == id)
              second.f64MinTransform
            else if (second.f64MinTransform == id)
              first.f64MinTransform
            else { (ctx: Ctx, t: f64.Min.type) =>
              first.f64MinTransform(ctx, t).flatMap {
                case (ctx, t: f64.Min.type) => second.f64MinTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64MaxTransform: (Ctx, f64.Max.type) => F[(Ctx, Inst)] =
            if (first.f64MaxTransform == id)
              second.f64MaxTransform
            else if (second.f64MaxTransform == id)
              first.f64MaxTransform
            else { (ctx: Ctx, t: f64.Max.type) =>
              first.f64MaxTransform(ctx, t).flatMap {
                case (ctx, t: f64.Max.type) => second.f64MaxTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }
          override val f64CopysignTransform: (Ctx, f64.Copysign.type) => F[(Ctx, Inst)] =
            if (first.f64CopysignTransform == id)
              second.f64CopysignTransform
            else if (second.f64CopysignTransform == id)
              first.f64CopysignTransform
            else { (ctx: Ctx, t: f64.Copysign.type) =>
              first.f64CopysignTransform(ctx, t).flatMap {
                case (ctx, t: f64.Copysign.type) => second.f64CopysignTransform(ctx, t)
                case (ctx, t)                    => second.transform(ctx, t)
              }
            }
          override val f64EqTransform: (Ctx, f64.Eq.type) => F[(Ctx, Inst)] =
            if (first.f64EqTransform == id)
              second.f64EqTransform
            else if (second.f64EqTransform == id)
              first.f64EqTransform
            else { (ctx: Ctx, t: f64.Eq.type) =>
              first.f64EqTransform(ctx, t).flatMap {
                case (ctx, t: f64.Eq.type) => second.f64EqTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f64NeTransform: (Ctx, f64.Ne.type) => F[(Ctx, Inst)] =
            if (first.f64NeTransform == id)
              second.f64NeTransform
            else if (second.f64NeTransform == id)
              first.f64NeTransform
            else { (ctx: Ctx, t: f64.Ne.type) =>
              first.f64NeTransform(ctx, t).flatMap {
                case (ctx, t: f64.Ne.type) => second.f64NeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f64LtTransform: (Ctx, f64.Lt.type) => F[(Ctx, Inst)] =
            if (first.f64LtTransform == id)
              second.f64LtTransform
            else if (second.f64LtTransform == id)
              first.f64LtTransform
            else { (ctx: Ctx, t: f64.Lt.type) =>
              first.f64LtTransform(ctx, t).flatMap {
                case (ctx, t: f64.Lt.type) => second.f64LtTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f64GtTransform: (Ctx, f64.Gt.type) => F[(Ctx, Inst)] =
            if (first.f64GtTransform == id)
              second.f64GtTransform
            else if (second.f64GtTransform == id)
              first.f64GtTransform
            else { (ctx: Ctx, t: f64.Gt.type) =>
              first.f64GtTransform(ctx, t).flatMap {
                case (ctx, t: f64.Gt.type) => second.f64GtTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f64LeTransform: (Ctx, f64.Le.type) => F[(Ctx, Inst)] =
            if (first.f64LeTransform == id)
              second.f64LeTransform
            else if (second.f64LeTransform == id)
              first.f64LeTransform
            else { (ctx: Ctx, t: f64.Le.type) =>
              first.f64LeTransform(ctx, t).flatMap {
                case (ctx, t: f64.Le.type) => second.f64LeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f64GeTransform: (Ctx, f64.Ge.type) => F[(Ctx, Inst)] =
            if (first.f64GeTransform == id)
              second.f64GeTransform
            else if (second.f64GeTransform == id)
              first.f64GeTransform
            else { (ctx: Ctx, t: f64.Ge.type) =>
              first.f64GeTransform(ctx, t).flatMap {
                case (ctx, t: f64.Ge.type) => second.f64GeTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val f64PromoteF32Transform: (Ctx, f64.PromoteF32.type) => F[(Ctx, Inst)] =
            if (first.f64PromoteF32Transform == id)
              second.f64PromoteF32Transform
            else if (second.f64PromoteF32Transform == id)
              first.f64PromoteF32Transform
            else { (ctx: Ctx, t: f64.PromoteF32.type) =>
              first.f64PromoteF32Transform(ctx, t).flatMap {
                case (ctx, t: f64.PromoteF32.type) => second.f64PromoteF32Transform(ctx, t)
                case (ctx, t)                      => second.transform(ctx, t)
              }
            }
          override val f64ConvertSI32Transform: (Ctx, f64.ConvertSI32.type) => F[(Ctx, Inst)] =
            if (first.f64ConvertSI32Transform == id)
              second.f64ConvertSI32Transform
            else if (second.f64ConvertSI32Transform == id)
              first.f64ConvertSI32Transform
            else { (ctx: Ctx, t: f64.ConvertSI32.type) =>
              first.f64ConvertSI32Transform(ctx, t).flatMap {
                case (ctx, t: f64.ConvertSI32.type) => second.f64ConvertSI32Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f64ConvertUI32Transform: (Ctx, f64.ConvertUI32.type) => F[(Ctx, Inst)] =
            if (first.f64ConvertUI32Transform == id)
              second.f64ConvertUI32Transform
            else if (second.f64ConvertUI32Transform == id)
              first.f64ConvertUI32Transform
            else { (ctx: Ctx, t: f64.ConvertUI32.type) =>
              first.f64ConvertUI32Transform(ctx, t).flatMap {
                case (ctx, t: f64.ConvertUI32.type) => second.f64ConvertUI32Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f64ConvertSI64Transform: (Ctx, f64.ConvertSI64.type) => F[(Ctx, Inst)] =
            if (first.f64ConvertSI64Transform == id)
              second.f64ConvertSI64Transform
            else if (second.f64ConvertSI64Transform == id)
              first.f64ConvertSI64Transform
            else { (ctx: Ctx, t: f64.ConvertSI64.type) =>
              first.f64ConvertSI64Transform(ctx, t).flatMap {
                case (ctx, t: f64.ConvertSI64.type) => second.f64ConvertSI64Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f64ConvertUI64Transform: (Ctx, f64.ConvertUI64.type) => F[(Ctx, Inst)] =
            if (first.f64ConvertUI64Transform == id)
              second.f64ConvertUI64Transform
            else if (second.f64ConvertUI64Transform == id)
              first.f64ConvertUI64Transform
            else { (ctx: Ctx, t: f64.ConvertUI64.type) =>
              first.f64ConvertUI64Transform(ctx, t).flatMap {
                case (ctx, t: f64.ConvertUI64.type) => second.f64ConvertUI64Transform(ctx, t)
                case (ctx, t)                       => second.transform(ctx, t)
              }
            }
          override val f64ReinterpretI64Transform: (Ctx, f64.ReinterpretI64.type) => F[(Ctx, Inst)] =
            if (first.f64ReinterpretI64Transform == id)
              second.f64ReinterpretI64Transform
            else if (second.f64ReinterpretI64Transform == id)
              first.f64ReinterpretI64Transform
            else { (ctx: Ctx, t: f64.ReinterpretI64.type) =>
              first.f64ReinterpretI64Transform(ctx, t).flatMap {
                case (ctx, t: f64.ReinterpretI64.type) => second.f64ReinterpretI64Transform(ctx, t)
                case (ctx, t)                          => second.transform(ctx, t)
              }
            }
          override val f64LoadTransform: (Ctx, f64.Load) => F[(Ctx, Inst)] =
            if (first.f64LoadTransform == id)
              second.f64LoadTransform
            else if (second.f64LoadTransform == id)
              first.f64LoadTransform
            else { (ctx: Ctx, t: f64.Load) =>
              first.f64LoadTransform(ctx, t).flatMap {
                case (ctx, t: f64.Load) => second.f64LoadTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val f64StoreTransform: (Ctx, f64.Store) => F[(Ctx, Inst)] =
            if (first.f64StoreTransform == id)
              second.f64StoreTransform
            else if (second.f64StoreTransform == id)
              first.f64StoreTransform
            else { (ctx: Ctx, t: f64.Store) =>
              first.f64StoreTransform(ctx, t).flatMap {
                case (ctx, t: f64.Store) => second.f64StoreTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val dropTransform: (Ctx, Drop.type) => F[(Ctx, Inst)] =
            if (first.dropTransform == id)
              second.dropTransform
            else if (second.dropTransform == id)
              first.dropTransform
            else { (ctx: Ctx, t: Drop.type) =>
              first.dropTransform(ctx, t).flatMap {
                case (ctx, t: Drop.type) => second.dropTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val selectTransform: (Ctx, Select.type) => F[(Ctx, Inst)] =
            if (first.selectTransform == id)
              second.selectTransform
            else if (second.selectTransform == id)
              first.selectTransform
            else { (ctx: Ctx, t: Select.type) =>
              first.selectTransform(ctx, t).flatMap {
                case (ctx, t: Select.type) => second.selectTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val localGetTransform: (Ctx, LocalGet) => F[(Ctx, Inst)] =
            if (first.localGetTransform == id)
              second.localGetTransform
            else if (second.localGetTransform == id)
              first.localGetTransform
            else { (ctx: Ctx, t: LocalGet) =>
              first.localGetTransform(ctx, t).flatMap {
                case (ctx, t: LocalGet) => second.localGetTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val localSetTransform: (Ctx, LocalSet) => F[(Ctx, Inst)] =
            if (first.localSetTransform == id)
              second.localSetTransform
            else if (second.localSetTransform == id)
              first.localSetTransform
            else { (ctx: Ctx, t: LocalSet) =>
              first.localSetTransform(ctx, t).flatMap {
                case (ctx, t: LocalSet) => second.localSetTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val localTeeTransform: (Ctx, LocalTee) => F[(Ctx, Inst)] =
            if (first.localTeeTransform == id)
              second.localTeeTransform
            else if (second.localTeeTransform == id)
              first.localTeeTransform
            else { (ctx: Ctx, t: LocalTee) =>
              first.localTeeTransform(ctx, t).flatMap {
                case (ctx, t: LocalTee) => second.localTeeTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val globalGetTransform: (Ctx, GlobalGet) => F[(Ctx, Inst)] =
            if (first.globalGetTransform == id)
              second.globalGetTransform
            else if (second.globalGetTransform == id)
              first.globalGetTransform
            else { (ctx: Ctx, t: GlobalGet) =>
              first.globalGetTransform(ctx, t).flatMap {
                case (ctx, t: GlobalGet) => second.globalGetTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val globalSetTransform: (Ctx, GlobalSet) => F[(Ctx, Inst)] =
            if (first.globalSetTransform == id)
              second.globalSetTransform
            else if (second.globalSetTransform == id)
              first.globalSetTransform
            else { (ctx: Ctx, t: GlobalSet) =>
              first.globalSetTransform(ctx, t).flatMap {
                case (ctx, t: GlobalSet) => second.globalSetTransform(ctx, t)
                case (ctx, t)            => second.transform(ctx, t)
              }
            }
          override val memorySizeTransform: (Ctx, MemorySize.type) => F[(Ctx, Inst)] =
            if (first.memorySizeTransform == id)
              second.memorySizeTransform
            else if (second.memorySizeTransform == id)
              first.memorySizeTransform
            else { (ctx: Ctx, t: MemorySize.type) =>
              first.memorySizeTransform(ctx, t).flatMap {
                case (ctx, t: MemorySize.type) => second.memorySizeTransform(ctx, t)
                case (ctx, t)                  => second.transform(ctx, t)
              }
            }
          override val memoryGrowTransform: (Ctx, MemoryGrow.type) => F[(Ctx, Inst)] =
            if (first.memoryGrowTransform == id)
              second.memoryGrowTransform
            else if (second.memoryGrowTransform == id)
              first.memoryGrowTransform
            else { (ctx: Ctx, t: MemoryGrow.type) =>
              first.memoryGrowTransform(ctx, t).flatMap {
                case (ctx, t: MemoryGrow.type) => second.memoryGrowTransform(ctx, t)
                case (ctx, t)                  => second.transform(ctx, t)
              }
            }
          override val nopTransform: (Ctx, Nop.type) => F[(Ctx, Inst)] =
            if (first.nopTransform == id)
              second.nopTransform
            else if (second.nopTransform == id)
              first.nopTransform
            else { (ctx: Ctx, t: Nop.type) =>
              first.nopTransform(ctx, t).flatMap {
                case (ctx, t: Nop.type) => second.nopTransform(ctx, t)
                case (ctx, t)           => second.transform(ctx, t)
              }
            }
          override val unreachableTransform: (Ctx, Unreachable.type) => F[(Ctx, Inst)] =
            if (first.unreachableTransform == id)
              second.unreachableTransform
            else if (second.unreachableTransform == id)
              first.unreachableTransform
            else { (ctx: Ctx, t: Unreachable.type) =>
              first.unreachableTransform(ctx, t).flatMap {
                case (ctx, t: Unreachable.type) => second.unreachableTransform(ctx, t)
                case (ctx, t)                   => second.transform(ctx, t)
              }
            }
          override val blockTransform: (Ctx, Block) => F[(Ctx, Inst)] =
            if (first.blockTransform == id)
              second.blockTransform
            else if (second.blockTransform == id)
              first.blockTransform
            else { (ctx: Ctx, t: Block) =>
              first.blockTransform(ctx, t).flatMap {
                case (ctx, t: Block) => second.blockTransform(ctx, t)
                case (ctx, t)        => second.transform(ctx, t)
              }
            }
          override val loopTransform: (Ctx, Loop) => F[(Ctx, Inst)] =
            if (first.loopTransform == id)
              second.loopTransform
            else if (second.loopTransform == id)
              first.loopTransform
            else { (ctx: Ctx, t: Loop) =>
              first.loopTransform(ctx, t).flatMap {
                case (ctx, t: Loop) => second.loopTransform(ctx, t)
                case (ctx, t)       => second.transform(ctx, t)
              }
            }
          override val ifTransform: (Ctx, If) => F[(Ctx, Inst)] =
            if (first.ifTransform == id)
              second.ifTransform
            else if (second.ifTransform == id)
              first.ifTransform
            else { (ctx: Ctx, t: If) =>
              first.ifTransform(ctx, t).flatMap {
                case (ctx, t: If) => second.ifTransform(ctx, t)
                case (ctx, t)     => second.transform(ctx, t)
              }
            }
          override val brTransform: (Ctx, Br) => F[(Ctx, Inst)] =
            if (first.brTransform == id)
              second.brTransform
            else if (second.brTransform == id)
              first.brTransform
            else { (ctx: Ctx, t: Br) =>
              first.brTransform(ctx, t).flatMap {
                case (ctx, t: Br) => second.brTransform(ctx, t)
                case (ctx, t)     => second.transform(ctx, t)
              }
            }
          override val brIfTransform: (Ctx, BrIf) => F[(Ctx, Inst)] =
            if (first.brIfTransform == id)
              second.brIfTransform
            else if (second.brIfTransform == id)
              first.brIfTransform
            else { (ctx: Ctx, t: BrIf) =>
              first.brIfTransform(ctx, t).flatMap {
                case (ctx, t: BrIf) => second.brIfTransform(ctx, t)
                case (ctx, t)       => second.transform(ctx, t)
              }
            }
          override val brTableTransform: (Ctx, BrTable) => F[(Ctx, Inst)] =
            if (first.brTableTransform == id)
              second.brTableTransform
            else if (second.brTableTransform == id)
              first.brTableTransform
            else { (ctx: Ctx, t: BrTable) =>
              first.brTableTransform(ctx, t).flatMap {
                case (ctx, t: BrTable) => second.brTableTransform(ctx, t)
                case (ctx, t)          => second.transform(ctx, t)
              }
            }
          override val returnTransform: (Ctx, Return.type) => F[(Ctx, Inst)] =
            if (first.returnTransform == id)
              second.returnTransform
            else if (second.returnTransform == id)
              first.returnTransform
            else { (ctx: Ctx, t: Return.type) =>
              first.returnTransform(ctx, t).flatMap {
                case (ctx, t: Return.type) => second.returnTransform(ctx, t)
                case (ctx, t)              => second.transform(ctx, t)
              }
            }
          override val callTransform: (Ctx, Call) => F[(Ctx, Inst)] =
            if (first.callTransform == id)
              second.callTransform
            else if (second.callTransform == id)
              first.callTransform
            else { (ctx: Ctx, t: Call) =>
              first.callTransform(ctx, t).flatMap {
                case (ctx, t: Call) => second.callTransform(ctx, t)
                case (ctx, t)       => second.transform(ctx, t)
              }
            }
          override val callIndirectTransform: (Ctx, CallIndirect) => F[(Ctx, Inst)] =
            if (first.callIndirectTransform == id)
              second.callIndirectTransform
            else if (second.callIndirectTransform == id)
              first.callIndirectTransform
            else { (ctx: Ctx, t: CallIndirect) =>
              first.callIndirectTransform(ctx, t).flatMap {
                case (ctx, t: CallIndirect) => second.callIndirectTransform(ctx, t)
                case (ctx, t)               => second.transform(ctx, t)
              }
            }

          override val blockPrepare: (Ctx, Block) => F[Ctx] =
            if (first.blockPrepare == fst)
              second.blockPrepare
            else if (second.blockPrepare == fst)
              first.blockPrepare
            else { (ctx: Ctx, t: Block) => first.blockPrepare(ctx, t).flatMap(second.blockPrepare(_, t)) }
          override val loopPrepare: (Ctx, Loop) => F[Ctx] =
            if (first.loopPrepare == fst)
              second.loopPrepare
            else if (second.loopPrepare == fst)
              first.loopPrepare
            else { (ctx: Ctx, t: Loop) => first.loopPrepare(ctx, t).flatMap(second.loopPrepare(_, t)) }
          override val ifPrepare: (Ctx, If) => F[Ctx] =
            if (first.ifPrepare == fst)
              second.ifPrepare
            else if (second.ifPrepare == fst)
              first.ifPrepare
            else { (ctx: Ctx, t: If) => first.ifPrepare(ctx, t).flatMap(second.ifPrepare(_, t)) }
          override val otherPrepare: (Ctx, Inst) => F[Ctx] =
            if (first.otherPrepare == fst)
              second.otherPrepare
            else if (second.otherPrepare == fst)
              first.otherPrepare
            else { (ctx: Ctx, t: Inst) => first.otherPrepare(ctx, t).flatMap(second.otherPrepare(_, t)) }

        }
    }
}
