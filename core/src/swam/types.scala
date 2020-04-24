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

sealed trait Type {

  def <:<(that: Type): Boolean

}

sealed abstract class ValType(val width: Int) extends Type

object ValType {
  case object I32 extends ValType(32) {
    def <:<(that: Type): Boolean =
      that == I32
  }
  case object I64 extends ValType(64) {
    def <:<(that: Type): Boolean =
      that == I64
  }
  case object F32 extends ValType(32) {
    def <:<(that: Type): Boolean =
      that == F32
  }
  case object F64 extends ValType(64) {
    def <:<(that: Type): Boolean =
      that == F64
  }
}

case class ResultType(t: Vector[ValType]) extends Type {
  def <:<(that: Type): Boolean =
    (this, that) match {
      case (ResultType(ts1), ResultType(ts2)) => ts1.size == ts2.size && ts1.zip(ts2).forall(ts => ts._1 <:< ts._2)
      case _                                  => false
    }
  def arity = t.size
}

case class FuncType(params: Vector[ValType], t: Vector[ValType]) extends Type {
  def <:<(that: Type): Boolean =
    that match {
      case FuncType(params1, t1) =>
        params.size == params1.size && t.size == t1.size && params1.zip(params).forall(ts => ts._1 <:< ts._2) && t1
          .zip(t)
          .forall(ts => ts._1 <:< ts._2)
      case _ =>
        false
    }
}

case class Limits(min: Int, max: Option[Int]) {
  def <:<(that: Limits): Boolean =
    (this, that) match {
      case (Limits(min, _), Limits(min1, None))               => min >= min1
      case (Limits(min, Some(max)), Limits(min1, Some(max1))) => min >= min1 && max <= max1
      case _                                                  => false
    }
}

object Limits extends ((Int, Int) => Limits) {
  def apply(min: Int, max: Int): Limits =
    Limits(min, Some(max))
}

case class MemType(limits: Limits) extends Type {
  def <:<(that: Type): Boolean =
    that match {
      case MemType(limits1) => limits <:< limits1
      case _                => false
    }
}

case class TableType(elemtype: ElemType, limits: Limits) extends Type {
  def <:<(that: Type): Boolean =
    that match {
      case TableType(elemtype1, limits1) => elemtype <:< elemtype1 && limits <:< limits1
      case _                             => false
    }
}

sealed trait ElemType extends Type

object ElemType {
  case object FuncRef extends ElemType {
    def <:<(that: Type): Boolean = true
  }
}

case class GlobalType(tpe: ValType, mut: Mut) extends Type {
  def <:<(that: Type): Boolean =
    that match {
      case GlobalType(tpe1, mut1) => mut == mut1 && tpe <:< tpe1
      case _                      => false
    }
}

sealed abstract class Mut(private val level: Int) extends Ordered[Mut] {
  def compare(that: Mut): Int =
    this.level - that.level
}

object Mut {
  case object Const extends Mut(1)
  case object Var extends Mut(0)
}

sealed trait BlockType {
  def params(funcs: Vector[FuncType]): Option[Vector[ValType]]
  def arity(funcs: Vector[FuncType]): Int
}
object BlockType {
  case object NoType extends BlockType {
    def params(funcs: Vector[FuncType]): Option[Vector[ValType]] = Some(Vector.empty)
    def arity(funcs: Vector[FuncType]): Int = 0
  }
  case class ValueType(tpe: ValType) extends BlockType {
    def params(funcs: Vector[FuncType]): Option[Vector[ValType]] = Some(Vector.empty)
    def arity(funcs: Vector[FuncType]): Int = 1
  }
  case class FunctionType(tpe: TypeIdx) extends BlockType {
    def params(funcs: Vector[FuncType]): Option[Vector[ValType]] =
      funcs.lift(tpe).map(_.params)
    def arity(funcs: Vector[FuncType]): Int =
      funcs.lift(tpe).map(_.t.size).getOrElse(-1)
  }
}
