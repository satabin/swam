package swam
package witx
package unresolved

import swam.witx.traverser.TypesTraverser

/**
  *@author Javier Cabrera-Arteaga on 2020-03-18
  */
abstract class BaseWitxType(val tpeName: String, val size: Int = 4)

sealed abstract class BasicType(name: String, override val size: Int) extends BaseWitxType(name, size)

object BasicType {
  case object u8 extends BasicType("u8", 1)
  case object u32 extends BasicType("u32", 4)
  case object u64 extends BasicType("u64", 8)
  case object u16 extends BasicType("u16", 2)
  case object s64 extends BasicType("s64", 8)
  case object string extends BasicType("string", 4)
  case object ptr extends BasicType("ptr", 4)
}

case class AliasType(override val tpeName: String, tpe: BaseWitxType) extends BaseWitxType(tpeName, size = tpe.size)

case class EnumType(override val tpeName: String, tpe: BaseWitxType, names: Seq[String])
    extends BaseWitxType(tpeName, size = tpe.size)

case class FlagsType(override val tpeName: String, tpe: BaseWitxType, names: Seq[String])
    extends BaseWitxType(tpeName, size = tpe.size)

case class ArrayType(override val tpeName: String, tpe: BaseWitxType) extends BaseWitxType(tpeName, size = tpe.size)

case class Pointer(tpe: BaseWitxType) extends BaseWitxType("Pointer", 4 * tpe.size) // 8 bytes

case class StructType(override val tpeName: String, fields: Seq[Field])
    extends BaseWitxType(tpeName, size = fields.map(t => t.tpe.size).sum)

case class UnionType(override val tpeName: String, name: String, fields: Seq[Field])
    extends BaseWitxType(tpeName, size = fields.map(t => t.tpe.size).sum)

case class Handle(override val tpeName: String) extends BaseWitxType(tpeName)
