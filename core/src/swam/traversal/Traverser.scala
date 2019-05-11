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
package traversal

import syntax._

import cats._
import cats.implicits._

import scala.language.higherKinds

/** Traverses a WebAssembly program and computes some result.
  * Tree traversal is performed in bottom-up order, with a _preparation_ phase
  * for recursive instructions to upgrade the context before inspecting children.
  *
  * Every instruction has a default noop `traverse` method, as well as a instruction categories
  * (see group `Category` for [[swam.syntax.Inst instructions]]).
  *
  * To implement your own traversal, you can override the methods that need specific handling.
  *
  */
class Traverser[F[_], Res](implicit F: Monad[F]) {

  protected[this] val fst = (res: Res, i: Inst) => F.pure(res)

  val constTraverse: (Res, AConst) => F[Res] = {
    case (res, c @ i32.Const(_)) => i32ConstTraverse(res, c)
    case (res, c @ i64.Const(_)) => i64ConstTraverse(res, c)
    case (res, c @ f32.Const(_)) => f32ConstTraverse(res, c)
    case (res, c @ f64.Const(_)) => f64ConstTraverse(res, c)
  }
  val unopTraverse: (Res, Unop) => F[Res] = {
    case (res, u @ i32.Clz)     => i32ClzTraverse(res, u)
    case (res, u @ i32.Ctz)     => i32CtzTraverse(res, u)
    case (res, u @ i32.Popcnt)  => i32PopcntTraverse(res, u)
    case (res, u @ i64.Clz)     => i64ClzTraverse(res, u)
    case (res, u @ i64.Ctz)     => i64CtzTraverse(res, u)
    case (res, u @ i64.Popcnt)  => i64PopcntTraverse(res, u)
    case (res, u @ f32.Abs)     => f32AbsTraverse(res, u)
    case (res, u @ f32.Neg)     => f32NegTraverse(res, u)
    case (res, u @ f32.Sqrt)    => f32SqrtTraverse(res, u)
    case (res, u @ f32.Ceil)    => f32CeilTraverse(res, u)
    case (res, u @ f32.Floor)   => f32FloorTraverse(res, u)
    case (res, u @ f32.Trunc)   => f32TruncTraverse(res, u)
    case (res, u @ f32.Nearest) => f32NearestTraverse(res, u)
    case (res, u @ f64.Abs)     => f64AbsTraverse(res, u)
    case (res, u @ f64.Neg)     => f64NegTraverse(res, u)
    case (res, u @ f64.Sqrt)    => f64SqrtTraverse(res, u)
    case (res, u @ f64.Ceil)    => f64CeilTraverse(res, u)
    case (res, u @ f64.Floor)   => f64FloorTraverse(res, u)
    case (res, u @ f64.Trunc)   => f64TruncTraverse(res, u)
    case (res, u @ f64.Nearest) => f64NearestTraverse(res, u)
  }
  val binopTraverse: (Res, Binop) => F[Res] = {
    case (res, b @ i32.Add)      => i32AddTraverse(res, b)
    case (res, b @ i32.Sub)      => i32SubTraverse(res, b)
    case (res, b @ i32.Mul)      => i32MulTraverse(res, b)
    case (res, b @ i32.DivS)     => i32DivSTraverse(res, b)
    case (res, b @ i32.DivU)     => i32DivUTraverse(res, b)
    case (res, b @ i32.RemS)     => i32RemSTraverse(res, b)
    case (res, b @ i32.RemU)     => i32RemUTraverse(res, b)
    case (res, b @ i32.And)      => i32AndTraverse(res, b)
    case (res, b @ i32.Or)       => i32OrTraverse(res, b)
    case (res, b @ i32.Xor)      => i32XorTraverse(res, b)
    case (res, b @ i32.Shl)      => i32ShlTraverse(res, b)
    case (res, b @ i32.ShrS)     => i32ShrSTraverse(res, b)
    case (res, b @ i32.ShrU)     => i32ShrUTraverse(res, b)
    case (res, b @ i32.Rotl)     => i32RotlTraverse(res, b)
    case (res, b @ i32.Rotr)     => i32RotrTraverse(res, b)
    case (res, b @ i64.Add)      => i64AddTraverse(res, b)
    case (res, b @ i64.Sub)      => i64SubTraverse(res, b)
    case (res, b @ i64.Mul)      => i64MulTraverse(res, b)
    case (res, b @ i64.DivS)     => i64DivSTraverse(res, b)
    case (res, b @ i64.DivU)     => i64DivUTraverse(res, b)
    case (res, b @ i64.RemS)     => i64RemSTraverse(res, b)
    case (res, b @ i64.RemU)     => i64RemUTraverse(res, b)
    case (res, b @ i64.And)      => i64AndTraverse(res, b)
    case (res, b @ i64.Or)       => i64OrTraverse(res, b)
    case (res, b @ i64.Xor)      => i64XorTraverse(res, b)
    case (res, b @ i64.Shl)      => i64ShlTraverse(res, b)
    case (res, b @ i64.ShrS)     => i64ShrSTraverse(res, b)
    case (res, b @ i64.ShrU)     => i64ShrUTraverse(res, b)
    case (res, b @ i64.Rotl)     => i64RotlTraverse(res, b)
    case (res, b @ i64.Rotr)     => i64RotrTraverse(res, b)
    case (res, b @ f32.Add)      => f32AddTraverse(res, b)
    case (res, b @ f32.Sub)      => f32SubTraverse(res, b)
    case (res, b @ f32.Mul)      => f32MulTraverse(res, b)
    case (res, b @ f32.Div)      => f32DivTraverse(res, b)
    case (res, b @ f32.Min)      => f32MinTraverse(res, b)
    case (res, b @ f32.Max)      => f32MaxTraverse(res, b)
    case (res, b @ f32.Copysign) => f32CopysignTraverse(res, b)
    case (res, b @ f64.Add)      => f64AddTraverse(res, b)
    case (res, b @ f64.Sub)      => f64SubTraverse(res, b)
    case (res, b @ f64.Mul)      => f64MulTraverse(res, b)
    case (res, b @ f64.Div)      => f64DivTraverse(res, b)
    case (res, b @ f64.Min)      => f64MinTraverse(res, b)
    case (res, b @ f64.Max)      => f64MaxTraverse(res, b)
    case (res, b @ f64.Copysign) => f64CopysignTraverse(res, b)
  }
  val testopTraverse: (Res, Testop) => F[Res] = {
    case (res, t @ i32.Eqz) => i32EqzTraverse(res, t)
    case (res, t @ i64.Eqz) => i64EqzTraverse(res, t)
  }
  val convertopTraverse: (Res, Convertop) => F[Res] = {
    case (res, c @ i32.WrapI64)        => i32WrapI64Traverse(res, c)
    case (res, c @ i32.TruncSF32)      => i32TruncSF32Traverse(res, c)
    case (res, c @ i32.TruncUF32)      => i32TruncUF32Traverse(res, c)
    case (res, c @ i32.TruncSF64)      => i32TruncSF64Traverse(res, c)
    case (res, c @ i32.TruncUF64)      => i32TruncUF64Traverse(res, c)
    case (res, c @ i32.ReinterpretF32) => i32ReinterpretF32Traverse(res, c)
    case (res, c @ i64.ExtendSI32)     => i64ExtendSI32Traverse(res, c)
    case (res, c @ i64.ExtendUI32)     => i64ExtendUI32Traverse(res, c)
    case (res, c @ i64.TruncSF32)      => i64TruncSF32Traverse(res, c)
    case (res, c @ i64.TruncUF32)      => i64TruncUF32Traverse(res, c)
    case (res, c @ i64.TruncSF64)      => i64TruncSF64Traverse(res, c)
    case (res, c @ i64.TruncUF64)      => i64TruncUF64Traverse(res, c)
    case (res, c @ i64.ReinterpretF64) => i64ReinterpretF64Traverse(res, c)
    case (res, c @ f32.DemoteF64)      => f32DemoteF64Traverse(res, c)
    case (res, c @ f32.ConvertSI32)    => f32ConvertSI32Traverse(res, c)
    case (res, c @ f32.ConvertUI32)    => f32ConvertUI32Traverse(res, c)
    case (res, c @ f32.ConvertSI64)    => f32ConvertSI64Traverse(res, c)
    case (res, c @ f32.ConvertUI64)    => f32ConvertUI64Traverse(res, c)
    case (res, c @ f32.ReinterpretI32) => f32ReinterpretI32Traverse(res, c)
    case (res, c @ f64.PromoteF32)     => f64PromoteF32Traverse(res, c)
    case (res, c @ f64.ConvertSI32)    => f64ConvertSI32Traverse(res, c)
    case (res, c @ f64.ConvertUI32)    => f64ConvertUI32Traverse(res, c)
    case (res, c @ f64.ConvertSI64)    => f64ConvertSI64Traverse(res, c)
    case (res, c @ f64.ConvertUI64)    => f64ConvertUI64Traverse(res, c)
    case (res, c @ f64.ReinterpretI64) => f64ReinterpretI64Traverse(res, c)
  }
  val memoryInstTraverse: (Res, MemoryInst) => F[Res] = {
    case (res, m @ Load(_, _, _))      => loadInstTraverse(res, m)
    case (res, m @ LoadN(_, _, _, _))  => loadNInstTraverse(res, m)
    case (res, m @ Store(_, _, _))     => storeInstTraverse(res, m)
    case (res, m @ StoreN(_, _, _, _)) => storeNInstTraverse(res, m)
  }
  val loadInstTraverse: (Res, LoadInst) => F[Res] = {
    case (res, l @ i32.Load(_, _)) => i32LoadTraverse(res, l)
    case (res, l @ i64.Load(_, _)) => i64LoadTraverse(res, l)
    case (res, l @ f32.Load(_, _)) => f32LoadTraverse(res, l)
    case (res, l @ f64.Load(_, _)) => f64LoadTraverse(res, l)
  }
  val loadNInstTraverse: (Res, LoadNInst) => F[Res] = {
    case (res, l @ i32.Load8S(_, _))  => i32Load8STraverse(res, l)
    case (res, l @ i32.Load8U(_, _))  => i32Load8UTraverse(res, l)
    case (res, l @ i32.Load16S(_, _)) => i32Load16STraverse(res, l)
    case (res, l @ i32.Load16U(_, _)) => i32Load16UTraverse(res, l)
    case (res, l @ i64.Load8S(_, _))  => i64Load8STraverse(res, l)
    case (res, l @ i64.Load8U(_, _))  => i64Load8UTraverse(res, l)
    case (res, l @ i64.Load16S(_, _)) => i64Load16STraverse(res, l)
    case (res, l @ i64.Load16U(_, _)) => i64Load16UTraverse(res, l)
    case (res, l @ i64.Load32S(_, _)) => i64Load32STraverse(res, l)
    case (res, l @ i64.Load32U(_, _)) => i64Load32UTraverse(res, l)
  }
  val storeInstTraverse: (Res, StoreInst) => F[Res] = {
    case (res, l @ i32.Store(_, _)) => i32StoreTraverse(res, l)
    case (res, l @ i64.Store(_, _)) => i64StoreTraverse(res, l)
    case (res, l @ f32.Store(_, _)) => f32StoreTraverse(res, l)
    case (res, l @ f64.Store(_, _)) => f64StoreTraverse(res, l)
  }
  val storeNInstTraverse: (Res, StoreNInst) => F[Res] = {
    case (res, l @ i32.Store8(_, _))  => i32Store8Traverse(res, l)
    case (res, l @ i32.Store16(_, _)) => i32Store16Traverse(res, l)
    case (res, l @ i64.Store8(_, _))  => i64Store8Traverse(res, l)
    case (res, l @ i64.Store16(_, _)) => i64Store16Traverse(res, l)
    case (res, l @ i64.Store32(_, _)) => i64Store32Traverse(res, l)
  }
  val relopTraverse: (Res, Relop) => F[Res] = {
    case (res, r @ i32.Eq)  => i32EqTraverse(res, r)
    case (res, r @ i32.Ne)  => i32NeTraverse(res, r)
    case (res, r @ i32.LtS) => i32LtSTraverse(res, r)
    case (res, r @ i32.LtU) => i32LtUTraverse(res, r)
    case (res, r @ i32.GtS) => i32GtSTraverse(res, r)
    case (res, r @ i32.GtU) => i32GtUTraverse(res, r)
    case (res, r @ i32.LeS) => i32LeSTraverse(res, r)
    case (res, r @ i32.LeU) => i32LeUTraverse(res, r)
    case (res, r @ i32.GeS) => i32GeSTraverse(res, r)
    case (res, r @ i32.GeU) => i32GeUTraverse(res, r)
    case (res, r @ i64.Eq)  => i64EqTraverse(res, r)
    case (res, r @ i64.Ne)  => i64NeTraverse(res, r)
    case (res, r @ i64.LtS) => i64LtSTraverse(res, r)
    case (res, r @ i64.LtU) => i64LtUTraverse(res, r)
    case (res, r @ i64.GtS) => i64GtSTraverse(res, r)
    case (res, r @ i64.GtU) => i64GtUTraverse(res, r)
    case (res, r @ i64.LeS) => i64LeSTraverse(res, r)
    case (res, r @ i64.LeU) => i64LeUTraverse(res, r)
    case (res, r @ i64.GeS) => i64GeSTraverse(res, r)
    case (res, r @ i64.GeU) => i64GeUTraverse(res, r)
    case (res, r @ f32.Eq)  => f32EqTraverse(res, r)
    case (res, r @ f32.Ne)  => f32NeTraverse(res, r)
    case (res, r @ f32.Lt)  => f32LtTraverse(res, r)
    case (res, r @ f32.Gt)  => f32GtTraverse(res, r)
    case (res, r @ f32.Le)  => f32LeTraverse(res, r)
    case (res, r @ f32.Ge)  => f32GeTraverse(res, r)
    case (res, r @ f64.Eq)  => f64EqTraverse(res, r)
    case (res, r @ f64.Ne)  => f64NeTraverse(res, r)
    case (res, r @ f64.Lt)  => f64LtTraverse(res, r)
    case (res, r @ f64.Gt)  => f64GtTraverse(res, r)
    case (res, r @ f64.Le)  => f64LeTraverse(res, r)
    case (res, r @ f64.Ge)  => f64GeTraverse(res, r)
  }
  val varInstTraverse: (Res, VarInst) => F[Res] = {
    case (res, v @ LocalGet(_))  => localGetTraverse(res, v)
    case (res, v @ LocalSet(_))  => localSetTraverse(res, v)
    case (res, v @ LocalTee(_))  => localTeeTraverse(res, v)
    case (res, v @ GlobalGet(_)) => globalGetTraverse(res, v)
    case (res, v @ GlobalSet(_)) => globalSetTraverse(res, v)
  }
  val i32ConstTraverse: (Res, i32.Const) => F[Res] = fst
  val i32ClzTraverse: (Res, i32.Clz.type) => F[Res] = fst
  val i32CtzTraverse: (Res, i32.Ctz.type) => F[Res] = fst
  val i32PopcntTraverse: (Res, i32.Popcnt.type) => F[Res] = fst
  val i32AddTraverse: (Res, i32.Add.type) => F[Res] = fst
  val i32SubTraverse: (Res, i32.Sub.type) => F[Res] = fst
  val i32MulTraverse: (Res, i32.Mul.type) => F[Res] = fst
  val i32DivSTraverse: (Res, i32.DivS.type) => F[Res] = fst
  val i32DivUTraverse: (Res, i32.DivU.type) => F[Res] = fst
  val i32RemSTraverse: (Res, i32.RemS.type) => F[Res] = fst
  val i32RemUTraverse: (Res, i32.RemU.type) => F[Res] = fst
  val i32AndTraverse: (Res, i32.And.type) => F[Res] = fst
  val i32OrTraverse: (Res, i32.Or.type) => F[Res] = fst
  val i32XorTraverse: (Res, i32.Xor.type) => F[Res] = fst
  val i32ShlTraverse: (Res, i32.Shl.type) => F[Res] = fst
  val i32ShrSTraverse: (Res, i32.ShrS.type) => F[Res] = fst
  val i32ShrUTraverse: (Res, i32.ShrU.type) => F[Res] = fst
  val i32RotlTraverse: (Res, i32.Rotl.type) => F[Res] = fst
  val i32RotrTraverse: (Res, i32.Rotr.type) => F[Res] = fst
  val i32EqzTraverse: (Res, i32.Eqz.type) => F[Res] = fst
  val i32EqTraverse: (Res, i32.Eq.type) => F[Res] = fst
  val i32NeTraverse: (Res, i32.Ne.type) => F[Res] = fst
  val i32LtSTraverse: (Res, i32.LtS.type) => F[Res] = fst
  val i32LtUTraverse: (Res, i32.LtU.type) => F[Res] = fst
  val i32GtSTraverse: (Res, i32.GtS.type) => F[Res] = fst
  val i32GtUTraverse: (Res, i32.GtU.type) => F[Res] = fst
  val i32LeSTraverse: (Res, i32.LeS.type) => F[Res] = fst
  val i32LeUTraverse: (Res, i32.LeU.type) => F[Res] = fst
  val i32GeSTraverse: (Res, i32.GeS.type) => F[Res] = fst
  val i32GeUTraverse: (Res, i32.GeU.type) => F[Res] = fst
  val i32WrapI64Traverse: (Res, i32.WrapI64.type) => F[Res] = fst
  val i32TruncSF32Traverse: (Res, i32.TruncSF32.type) => F[Res] = fst
  val i32TruncUF32Traverse: (Res, i32.TruncUF32.type) => F[Res] = fst
  val i32TruncSF64Traverse: (Res, i32.TruncSF64.type) => F[Res] = fst
  val i32TruncUF64Traverse: (Res, i32.TruncUF64.type) => F[Res] = fst
  val i32ReinterpretF32Traverse: (Res, i32.ReinterpretF32.type) => F[Res] = fst
  val i32LoadTraverse: (Res, i32.Load) => F[Res] = fst
  val i32StoreTraverse: (Res, i32.Store) => F[Res] = fst
  val i32Load8STraverse: (Res, i32.Load8S) => F[Res] = fst
  val i32Load8UTraverse: (Res, i32.Load8U) => F[Res] = fst
  val i32Load16STraverse: (Res, i32.Load16S) => F[Res] = fst
  val i32Load16UTraverse: (Res, i32.Load16U) => F[Res] = fst
  val i32Store8Traverse: (Res, i32.Store8) => F[Res] = fst
  val i32Store16Traverse: (Res, i32.Store16) => F[Res] = fst
  val i64ConstTraverse: (Res, i64.Const) => F[Res] = fst
  val i64ClzTraverse: (Res, i64.Clz.type) => F[Res] = fst
  val i64CtzTraverse: (Res, i64.Ctz.type) => F[Res] = fst
  val i64PopcntTraverse: (Res, i64.Popcnt.type) => F[Res] = fst
  val i64AddTraverse: (Res, i64.Add.type) => F[Res] = fst
  val i64SubTraverse: (Res, i64.Sub.type) => F[Res] = fst
  val i64MulTraverse: (Res, i64.Mul.type) => F[Res] = fst
  val i64DivSTraverse: (Res, i64.DivS.type) => F[Res] = fst
  val i64DivUTraverse: (Res, i64.DivU.type) => F[Res] = fst
  val i64RemSTraverse: (Res, i64.RemS.type) => F[Res] = fst
  val i64RemUTraverse: (Res, i64.RemU.type) => F[Res] = fst
  val i64AndTraverse: (Res, i64.And.type) => F[Res] = fst
  val i64OrTraverse: (Res, i64.Or.type) => F[Res] = fst
  val i64XorTraverse: (Res, i64.Xor.type) => F[Res] = fst
  val i64ShlTraverse: (Res, i64.Shl.type) => F[Res] = fst
  val i64ShrSTraverse: (Res, i64.ShrS.type) => F[Res] = fst
  val i64ShrUTraverse: (Res, i64.ShrU.type) => F[Res] = fst
  val i64RotlTraverse: (Res, i64.Rotl.type) => F[Res] = fst
  val i64RotrTraverse: (Res, i64.Rotr.type) => F[Res] = fst
  val i64EqzTraverse: (Res, i64.Eqz.type) => F[Res] = fst
  val i64EqTraverse: (Res, i64.Eq.type) => F[Res] = fst
  val i64NeTraverse: (Res, i64.Ne.type) => F[Res] = fst
  val i64LtSTraverse: (Res, i64.LtS.type) => F[Res] = fst
  val i64LtUTraverse: (Res, i64.LtU.type) => F[Res] = fst
  val i64GtSTraverse: (Res, i64.GtS.type) => F[Res] = fst
  val i64GtUTraverse: (Res, i64.GtU.type) => F[Res] = fst
  val i64LeSTraverse: (Res, i64.LeS.type) => F[Res] = fst
  val i64LeUTraverse: (Res, i64.LeU.type) => F[Res] = fst
  val i64GeSTraverse: (Res, i64.GeS.type) => F[Res] = fst
  val i64GeUTraverse: (Res, i64.GeU.type) => F[Res] = fst
  val i64ExtendSI32Traverse: (Res, i64.ExtendSI32.type) => F[Res] = fst
  val i64ExtendUI32Traverse: (Res, i64.ExtendUI32.type) => F[Res] = fst
  val i64TruncSF32Traverse: (Res, i64.TruncSF32.type) => F[Res] = fst
  val i64TruncUF32Traverse: (Res, i64.TruncUF32.type) => F[Res] = fst
  val i64TruncSF64Traverse: (Res, i64.TruncSF64.type) => F[Res] = fst
  val i64TruncUF64Traverse: (Res, i64.TruncUF64.type) => F[Res] = fst
  val i64ReinterpretF64Traverse: (Res, i64.ReinterpretF64.type) => F[Res] = fst
  val i64LoadTraverse: (Res, i64.Load) => F[Res] = fst
  val i64StoreTraverse: (Res, i64.Store) => F[Res] = fst
  val i64Load8STraverse: (Res, i64.Load8S) => F[Res] = fst
  val i64Load8UTraverse: (Res, i64.Load8U) => F[Res] = fst
  val i64Load16STraverse: (Res, i64.Load16S) => F[Res] = fst
  val i64Load16UTraverse: (Res, i64.Load16U) => F[Res] = fst
  val i64Load32STraverse: (Res, i64.Load32S) => F[Res] = fst
  val i64Load32UTraverse: (Res, i64.Load32U) => F[Res] = fst
  val i64Store8Traverse: (Res, i64.Store8) => F[Res] = fst
  val i64Store16Traverse: (Res, i64.Store16) => F[Res] = fst
  val i64Store32Traverse: (Res, i64.Store32) => F[Res] = fst
  val f32ConstTraverse: (Res, f32.Const) => F[Res] = fst
  val f32AbsTraverse: (Res, f32.Abs.type) => F[Res] = fst
  val f32NegTraverse: (Res, f32.Neg.type) => F[Res] = fst
  val f32SqrtTraverse: (Res, f32.Sqrt.type) => F[Res] = fst
  val f32CeilTraverse: (Res, f32.Ceil.type) => F[Res] = fst
  val f32FloorTraverse: (Res, f32.Floor.type) => F[Res] = fst
  val f32TruncTraverse: (Res, f32.Trunc.type) => F[Res] = fst
  val f32NearestTraverse: (Res, f32.Nearest.type) => F[Res] = fst
  val f32AddTraverse: (Res, f32.Add.type) => F[Res] = fst
  val f32SubTraverse: (Res, f32.Sub.type) => F[Res] = fst
  val f32MulTraverse: (Res, f32.Mul.type) => F[Res] = fst
  val f32DivTraverse: (Res, f32.Div.type) => F[Res] = fst
  val f32MinTraverse: (Res, f32.Min.type) => F[Res] = fst
  val f32MaxTraverse: (Res, f32.Max.type) => F[Res] = fst
  val f32CopysignTraverse: (Res, f32.Copysign.type) => F[Res] = fst
  val f32EqTraverse: (Res, f32.Eq.type) => F[Res] = fst
  val f32NeTraverse: (Res, f32.Ne.type) => F[Res] = fst
  val f32LtTraverse: (Res, f32.Lt.type) => F[Res] = fst
  val f32GtTraverse: (Res, f32.Gt.type) => F[Res] = fst
  val f32LeTraverse: (Res, f32.Le.type) => F[Res] = fst
  val f32GeTraverse: (Res, f32.Ge.type) => F[Res] = fst
  val f32DemoteF64Traverse: (Res, f32.DemoteF64.type) => F[Res] = fst
  val f32ConvertSI32Traverse: (Res, f32.ConvertSI32.type) => F[Res] = fst
  val f32ConvertUI32Traverse: (Res, f32.ConvertUI32.type) => F[Res] = fst
  val f32ConvertSI64Traverse: (Res, f32.ConvertSI64.type) => F[Res] = fst
  val f32ConvertUI64Traverse: (Res, f32.ConvertUI64.type) => F[Res] = fst
  val f32ReinterpretI32Traverse: (Res, f32.ReinterpretI32.type) => F[Res] = fst
  val f32LoadTraverse: (Res, f32.Load) => F[Res] = fst
  val f32StoreTraverse: (Res, f32.Store) => F[Res] = fst
  val f64ConstTraverse: (Res, f64.Const) => F[Res] = fst
  val f64AbsTraverse: (Res, f64.Abs.type) => F[Res] = fst
  val f64NegTraverse: (Res, f64.Neg.type) => F[Res] = fst
  val f64SqrtTraverse: (Res, f64.Sqrt.type) => F[Res] = fst
  val f64CeilTraverse: (Res, f64.Ceil.type) => F[Res] = fst
  val f64FloorTraverse: (Res, f64.Floor.type) => F[Res] = fst
  val f64TruncTraverse: (Res, f64.Trunc.type) => F[Res] = fst
  val f64NearestTraverse: (Res, f64.Nearest.type) => F[Res] = fst
  val f64AddTraverse: (Res, f64.Add.type) => F[Res] = fst
  val f64SubTraverse: (Res, f64.Sub.type) => F[Res] = fst
  val f64MulTraverse: (Res, f64.Mul.type) => F[Res] = fst
  val f64DivTraverse: (Res, f64.Div.type) => F[Res] = fst
  val f64MinTraverse: (Res, f64.Min.type) => F[Res] = fst
  val f64MaxTraverse: (Res, f64.Max.type) => F[Res] = fst
  val f64CopysignTraverse: (Res, f64.Copysign.type) => F[Res] = fst
  val f64EqTraverse: (Res, f64.Eq.type) => F[Res] = fst
  val f64NeTraverse: (Res, f64.Ne.type) => F[Res] = fst
  val f64LtTraverse: (Res, f64.Lt.type) => F[Res] = fst
  val f64GtTraverse: (Res, f64.Gt.type) => F[Res] = fst
  val f64LeTraverse: (Res, f64.Le.type) => F[Res] = fst
  val f64GeTraverse: (Res, f64.Ge.type) => F[Res] = fst
  val f64PromoteF32Traverse: (Res, f64.PromoteF32.type) => F[Res] = fst
  val f64ConvertSI32Traverse: (Res, f64.ConvertSI32.type) => F[Res] = fst
  val f64ConvertUI32Traverse: (Res, f64.ConvertUI32.type) => F[Res] = fst
  val f64ConvertSI64Traverse: (Res, f64.ConvertSI64.type) => F[Res] = fst
  val f64ConvertUI64Traverse: (Res, f64.ConvertUI64.type) => F[Res] = fst
  val f64ReinterpretI64Traverse: (Res, f64.ReinterpretI64.type) => F[Res] = fst
  val f64LoadTraverse: (Res, f64.Load) => F[Res] = fst
  val f64StoreTraverse: (Res, f64.Store) => F[Res] = fst
  val dropTraverse: (Res, Drop.type) => F[Res] = fst
  val selectTraverse: (Res, Select.type) => F[Res] = fst
  val localGetTraverse: (Res, LocalGet) => F[Res] = fst
  val localSetTraverse: (Res, LocalSet) => F[Res] = fst
  val localTeeTraverse: (Res, LocalTee) => F[Res] = fst
  val globalGetTraverse: (Res, GlobalGet) => F[Res] = fst
  val globalSetTraverse: (Res, GlobalSet) => F[Res] = fst
  val memorySizeTraverse: (Res, MemorySize.type) => F[Res] = fst
  val memoryGrowTraverse: (Res, MemoryGrow.type) => F[Res] = fst
  val nopTraverse: (Res, Nop.type) => F[Res] = fst
  val unreachableTraverse: (Res, Unreachable.type) => F[Res] = fst
  val blockTraverse: (Res, Block) => F[Res] = fst
  val loopTraverse: (Res, Loop) => F[Res] = fst
  val ifTraverse: (Res, If) => F[Res] = fst
  val brTraverse: (Res, Br) => F[Res] = fst
  val brIfTraverse: (Res, BrIf) => F[Res] = fst
  val brTableTraverse: (Res, BrTable) => F[Res] = fst
  val returnTraverse: (Res, Return.type) => F[Res] = fst
  val callTraverse: (Res, Call) => F[Res] = fst
  val callIndirectTraverse: (Res, CallIndirect) => F[Res] = fst

  val blockPrepare: (Res, Block) => F[Res] = fst
  val loopPrepare: (Res, Loop) => F[Res] = fst
  val thenPrepare: (Res, If) => F[Res] = fst
  val elsePrepare: (Res, If) => F[Res] = fst
  val otherPrepare: (Res, Inst) => F[Res] = fst

  final def traverse(zero: Res, inst: Inst): F[Res] = inst match {
    case inst @ AConst(_)        => constTraverse(zero, inst)
    case inst @ Unop(_)          => unopTraverse(zero, inst)
    case inst @ Binop(_)         => binopTraverse(zero, inst)
    case inst @ Testop(_)        => testopTraverse(zero, inst)
    case inst @ Relop(_)         => relopTraverse(zero, inst)
    case inst @ Convertop(_, _)  => convertopTraverse(zero, inst)
    case inst @ MemoryInst(_, _) => memoryInstTraverse(zero, inst)
    case inst @ Drop             => dropTraverse(zero, inst)
    case inst @ Select           => selectTraverse(zero, inst)
    case inst @ VarInst(_)       => varInstTraverse(zero, inst)
    case inst @ MemorySize       => memorySizeTraverse(zero, inst)
    case inst @ MemoryGrow       => memoryGrowTraverse(zero, inst)
    case inst @ Nop              => nopTraverse(zero, inst)
    case inst @ Unreachable      => unreachableTraverse(zero, inst)
    case inst @ Block(_, _)      => blockTraverse(zero, inst)
    case inst @ Loop(_, _)       => loopTraverse(zero, inst)
    case inst @ If(_, _, _)      => ifTraverse(zero, inst)
    case inst @ Br(_)            => brTraverse(zero, inst)
    case inst @ BrIf(_)          => brIfTraverse(zero, inst)
    case inst @ BrTable(_, _)    => brTableTraverse(zero, inst)
    case inst @ Return           => returnTraverse(zero, inst)
    case inst @ Call(_)          => callTraverse(zero, inst)
    case inst @ CallIndirect(_)  => callIndirectTraverse(zero, inst)
  }

  final def run(zero: Res, inst: Inst): F[Res] =
    inst match {
      case b @ Block(_, is) =>
        for {
          res <- blockPrepare(zero, b)
          res <- is.foldM(res)(run(_, _))
          res <- traverse(res, inst)
        } yield res
      case l @ Loop(_, is) =>
        for {
          res <- loopPrepare(zero, l)
          res <- is.foldM(res)(run(_, _))
          res <- traverse(res, inst)
        } yield res
      case i @ If(_, ts, es) =>
        for {
          res <- thenPrepare(zero, i)
          res <- ts.foldM(res)(run(_, _))
          res <- elsePrepare(res, i)
          res <- es.foldM(res)(run(_, _))
          res <- traverse(res, inst)
        } yield res
      case _ =>
        for {
          res <- otherPrepare(zero, inst)
          res <- traverse(res, inst)
        } yield res
    }

}
