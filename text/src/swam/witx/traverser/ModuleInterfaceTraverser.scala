package swam.witx.traverser

import cats.Monad
import swam.witx.unresolved._

/**
  * @author Javier Cabrera-Arteaga on 2020-03-21
  */
class ModuleInterfaceTraverser[Res](val interface: ModuleInterface) {

  protected[this] val fst = (res: Res, i: Declaration) => res

  val importDeclarationTraverser: (Res, ImportDeclaration) => Res = fst
  val functionExportTraverser: (Res, FunctionExport) => Res = fst

  def traverseAll(zero: Res, compose: (Res, Res) => Res) =
    interface.declarations
      .map(d => traverse(zero, d))
      .reduce(compose)

  def traverse(zero: Res, x: Declaration): Res = {
    x match {
      case x: FunctionExport    => functionExportTraverser(zero, x)
      case x: ImportDeclaration => importDeclarationTraverser(zero, x)
    }
  }

}
