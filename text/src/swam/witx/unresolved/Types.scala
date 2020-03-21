package swam
package witx
package unresolved

import swam.witx.traverser.TypesTraverser

/**
  *@author Javier Cabrera-Arteaga on 2020-03-18
  */
abstract class BaseWitxType(val tpeName: String)

case class BasicType(name: String) extends BaseWitxType(name)

case class AliasType(override val tpeName: String, name: String) extends BaseWitxType(tpeName)

case class EnumType(override val tpeName: String, tpe: BaseWitxType, names: Seq[String]) extends BaseWitxType(tpeName)

case class FlagsType(override val tpeName: String, tpe: BaseWitxType, names: Seq[String]) extends BaseWitxType(tpeName)

case class ArrayType(override val tpeName: String, tpe: BaseWitxType) extends BaseWitxType(tpeName)

case class Pointer(tpe: BaseWitxType) extends BaseWitxType("Pointer")

case class StructType(override val tpeName: String, fields: Seq[Field]) extends BaseWitxType(tpeName)

case class UnionType(override val tpeName: String, name: String, fields: Seq[Field]) extends BaseWitxType(tpeName)

case class Handle(override val tpeName: String) extends BaseWitxType(tpeName)
