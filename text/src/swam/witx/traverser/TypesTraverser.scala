package swam.witx.traverser

import cats.Monad
import cats.effect.Effect
import swam.syntax.{Inst, i64}
import swam.witx.unresolved._

/**
  * @author Javier Cabrera-Arteaga on 2020-03-21
  */
abstract class TypesTraverser[Res](val types: Map[String, BaseWitxType]) {

  protected[this] val fst = (res: Res, i: BaseWitxType) => res

  val aliasTypeTraverser: (Res, AliasType) => Res = fst
  val basicTypeTraverser: (Res, BasicType) => Res = fst
  val enumTypeTraverser: (Res, EnumType) => Res = fst
  val flagsTypeTraverser: (Res, FlagsType) => Res = fst
  val arrayTypeTraverser: (Res, ArrayType) => Res = fst
  val pointerTypeTraverser: (Res, Pointer) => Res = fst
  val structTypeTraverser: (Res, StructType) => Res = fst
  val unionTypeTraverser: (Res, UnionType) => Res = fst
  val handleTypeTraverser: (Res, Handle) => Res = fst

  def traverseAll(zero: Res, compose: (Res, Res) => Res) =
    types
      .map { case (name, t) => traverse(zero, t) }
      .reduce(compose)

  def traverse(zero: Res, x: BaseWitxType): Res = {
    x match {
      case x: AliasType  => aliasTypeTraverser(zero, x)
      case x: BasicType  => basicTypeTraverser(zero, x)
      case x: EnumType   => enumTypeTraverser(zero, x)
      case x: FlagsType  => flagsTypeTraverser(zero, x)
      case x: StructType => structTypeTraverser(zero, x)
      case x: UnionType  => unionTypeTraverser(zero, x)
      case x: Handle     => handleTypeTraverser(zero, x)
      case x: ArrayType  => arrayTypeTraverser(zero, x)
      case x: Pointer    => pointerTypeTraverser(zero, x)
    }
  }
}
