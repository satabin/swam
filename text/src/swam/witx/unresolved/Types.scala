package swam
package witx
package unresolved

/**
@author Javier Cabrera-Arteaga on 2020-03-18
  */
sealed trait BaseWitxType

case class BasicType(name: String) extends BaseWitxType

case class AliasType(name: String) extends BaseWitxType

case class EnumType(tpe: BaseWitxType, names: Seq[String]) extends BaseWitxType

case class FlagsType(tpe: BaseWitxType, names: Seq[String]) extends BaseWitxType

case class ArrayType(tpe: BaseWitxType) extends BaseWitxType

case class Pointer(tpe: BaseWitxType) extends BaseWitxType

case class StructType(fields: Seq[Field]) extends BaseWitxType

case class UnionType(name: String, fields: Seq[Field]) extends BaseWitxType

case class Handle() extends BaseWitxType

case class Field(id: String, tpe: BaseWitxType)

sealed trait Declaration

case class FunctionExport(id: String, params: Seq[Field], results: Seq[Field]) extends Declaration

case class ImportDeclaration(name: String, tpe: String) extends Declaration

case class ModuleInterface(id: String, declarations: Seq[Declaration]) extends Declaration
