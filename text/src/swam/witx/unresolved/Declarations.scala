package swam
package witx
package unresolved

/**
    @author Javier Cabrera-Arteaga on 2020-03-21
  */
sealed trait Declaration

case class Field(id: String, tpe: BaseWitxType) extends Declaration

case class FunctionExport(id: String, params: Seq[Field], results: Seq[Field]) extends Declaration

case class ImportDeclaration(name: String, tpe: String) extends Declaration

case class ModuleInterface(id: String, declarations: Seq[Declaration]) extends Declaration
