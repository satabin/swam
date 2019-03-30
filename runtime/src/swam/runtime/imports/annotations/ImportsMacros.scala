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
package runtime
package imports
package annotations

import scala.reflect.macros.blackbox

private final class ImportsMacros(val c: blackbox.Context) {
  import c.universe._

  def moduleMacro(annottees: Tree*): Tree = annottees match {
    case List(clsDef: ClassDef) =>
      q"""
       $clsDef
       object ${clsDef.name.toTermName} {
         import scala.language.higherKinds
         ${module(c.typecheck(clsDef))}
       }
       """
    case List(
        clsDef: ClassDef,
        q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        ) =>
      q"""
       $clsDef
       $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
         ..$objDefs
         import scala.language.higherKinds
         ${module(c.typecheck(clsDef))}
       }
       """
    case _ => c.abort(c.enclosingPosition, "Invalid annotation target: must be a class")
  }

  private val GlobalType = typeOf[swam.runtime.imports.annotations.global]

  private def isGlobal(a: Annotation): Boolean = a.tree.tpe match {
    case GlobalType => true
    case t          => false
  }

  private object ExportedName {
    def unapply(a: Annotation): Option[Tree] =
      a.tree.children.tail.collectFirst {
        case q"name = $name" => name
        case q"$name"        => name
      }
  }

  private def module(cls: Tree): Tree = cls match {
    case cls: ClassDef =>
      object Export {
        def makeGlobal(writable: Boolean)(sym: Symbol): Tree = {
          q"""F.pure(new _root_.swam.runtime.Global[F] {
            import _root_.cats.implicits._
        private val formatter = implicitly[_root_.swam.runtime.formats.SimpleValueFormatter[${sym.typeSignature}]]
        val tpe = _root_.swam.GlobalType(formatter.swamType, ${if (writable) q"_root_.swam.Mut.Var"
          else q"_root_.swam.Mut.Const"})
        def get: _root_.swam.runtime.Value = formatter.write(t.${TermName(sym.name.decodedName.toString.trim)})
        def set(v: _root_.swam.runtime.Value)(implicit F: _root_.cats.MonadError[F,Throwable]) = ${if (writable)
            q"formatter.read[F](v).map(t.${TermName(sym.name.decodedName.toString.trim)} = _)"
          else
            q"""F.raiseError(new RuntimeException("Unable to set immutable global"))"""}
      })"""
          //q"F.pure(implicitly[_root_.swam.runtime.imports.AsInterface[${sym.typeSignature}, F]].view(t.${TermName(sym.name.decodedName.toString.trim)}))"
        }

        private def makeExport(mods: Modifiers, sym: Symbol, writable: Boolean, pos: Position) = {
          val ename = sym.annotations
            .collectFirst {
              case ExportedName(name) => name
            }
            .getOrElse(atPos(pos)(q"""${sym.name.decodedName.toString.trim}"""))
          Some((ename, sym, makeGlobal(writable) _))
        }

        def unapply(a: Tree): Option[(Tree, Symbol, Symbol => Tree)] = a match {
          case t @ q"$mods val $tname: $tpt = $expr" if t.symbol.annotations.exists(isGlobal(_)) =>
            makeExport(mods, t.symbol, false, t.pos)
          case t @ q"$mods var $tname: $tpt = $expr" if t.symbol.annotations.exists(isGlobal(_)) =>
            makeExport(mods, t.symbol, true, t.pos)
          //case t @ q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" if t.symbol.annotations.exists(isExported(_)) =>
          //  makeExport(mods, t.symbol, t.pos)
          case t =>
            None
        }
      }

      // first start by collecting all fields marked as exported
      val ts =
        cls.collect {
          case Export(ename, name, writable) => (ename, name, writable)
        }

      checkNames(ts)

      val asInstanceName = c.freshName(TermName(s"${cls.name}AsInstance"))

      val cases = ts.map { case (ename, name, mk) => (cq"$ename => ${mk(name)}") }

      q"""implicit def $asInstanceName[F[_]]: _root_.swam.runtime.imports.AsInstance[${cls.name}, F] = new _root_.swam.runtime.imports.AsInstance[${cls.name}, F] {
      def find(t: ${cls.name}, field: String)(implicit F: _root_.cats.MonadError[F,Throwable]): F[_root_.swam.runtime.Interface[F, _root_.swam.Type]] =
        field match {
          case ..$cases
          case _ =>
            F.raiseError(new Exception(s"Unknown field $$field"))
        }
    }"""
    case _ =>
      c.abort(c.enclosingPosition, "This is a bug")
  }

  private def checkNames(ts: List[(Tree, Symbol, Symbol => Tree)]): Unit = {
    val byname =
      ts.map {
          case (l @ Literal(name), _, _) => l
          case (n, _, _)                 => c.abort(n.pos, s"Only string literals are allowed for exported field names, but got $n")
        }
        .groupBy(_.value)
    byname.find(_._2.size > 1).foreach {
      case (_, n :: rest) =>
        c.abort(n.pos,
                s"Field names must be unique (same name exported at line(s) ${rest.map(_.pos.line).mkString(", ")})")
      case (_, _) =>
        c.abort(c.enclosingPosition, "This is a bug")
    }
  }

}
