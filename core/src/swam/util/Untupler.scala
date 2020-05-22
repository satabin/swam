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
package util

import shapeless._
import shapeless.ops.function._

/** Typeclass to transform a function that accepts a single tuple
  * argument into a function that accepts several arguments of the
  * tuple types.
  */
trait Untupler[Fun] {
  type Out
  def untuple(f: Fun): Out
}

object Untupler {
  type Aux[Out0, Fun] = Untupler[Fun] { type Out = Out0 }

  implicit def generic[TupleIn, Ret, HListIn, Out0](
      implicit isTuple: IsTuple[TupleIn],
      gen: Generic.Aux[TupleIn, HListIn],
      fnFromProduct: FnFromProduct.Aux[HListIn => Ret, Out0]): Untupler.Aux[Out0, TupleIn => Ret] =
    new Untupler[TupleIn => Ret] {
      override type Out = Out0
      override def untuple(f: TupleIn => Ret): Out = fnFromProduct(tupleIn => f(gen.from(tupleIn)))
    }

  implicit def identity[T, Ret](implicit refute: Refute[IsTuple[T]]): Untupler.Aux[T => Ret, T => Ret] =
    new Untupler[T => Ret] {
      type Out = T => Ret
      def untuple(f: T => Ret): Out = f
    }

}
