package swam
package witx
package unresolved

/**
@author Javier Cabrera-Arteaga on 2020-03-18
  */
sealed trait BaseWitxType

class WitXType(val name: String, val `type`: BaseWitxType) {}

case class AliasType(name: String) extends BaseWitxType

case class EnumType(`type`: String, names: Seq[String]) extends BaseWitxType

case class FlagsType(`type`: String, names: Seq[String]) extends BaseWitxType

case class ArrayType(`type`: String) extends BaseWitxType

case class StructType(fields: Seq[Either[Field, PointerField]]) extends BaseWitxType

case class UnionType(name: String, fields: Seq[Either[Field, PointerField]]) extends BaseWitxType

case class Handle() extends BaseWitxType

case class Field(id: String, `type`: String)

case class Pointer(`type`: String)

case class PointerField(id: String, pointer: Pointer)
