/*
 * Copyright 2019 Lucas Satabin
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
      val typed = c.typecheck(clsDef)
      q"""
       $clsDef
       object ${clsDef.name.toTermName} {
         import scala.language.higherKinds
         ${module(typed)}
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
  private val PureType = typeOf[swam.runtime.imports.annotations.pure]
  private val EffectfulType = typeOf[swam.runtime.imports.annotations.effectful]
  private val EffectType = typeOf[swam.runtime.imports.annotations.effect]

  private def isGlobal(a: Annotation): Boolean = a.tree.tpe match {
    case GlobalType => true
    case t          => false
  }

  private def isPure(a: Annotation): Boolean = a.tree.tpe match {
    case PureType => true
    case t        => false
  }

  private def isEffectful(a: Annotation): Boolean = a.tree.tpe match {
    case EffectfulType => true
    case t             => false
  }

  private def isEffect(a: Annotation): Boolean = a.tree.tpe match {
    case EffectType => true
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
    case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
      val (efftpe, f) = tparams
        .collectFirst {
          case t if t.symbol.annotations.exists(isEffect(_)) => (Seq.empty[TypeDef], t.symbol.name.toTypeName)
        }
        .getOrElse {
          val f = c.freshName(TypeName("F"))
          (Seq(q"type $f[_]"), f)
        }

      object Export {
        def makeGlobal(writable: Boolean)(sym: Symbol): Tree = {
          q"""F.pure(new _root_.swam.runtime.Global[$f] {
        private val formatter = implicitly[_root_.swam.runtime.formats.SimpleValueFormatter[$f, ${sym.typeSignature}]]
        val tpe = _root_.swam.GlobalType(formatter.swamType, ${if (writable) q"_root_.swam.Mut.Var"
          else q"_root_.swam.Mut.Const"})
        def get: _root_.swam.runtime.Value = formatter.write(t.${TermName(sym.name.decodedName.toString.trim)})
        def set(v: _root_.swam.runtime.Value) = ${if (writable)
            q"formatter.read(v).map(t.${TermName(sym.name.decodedName.toString.trim)} = _)"
          else
            q"""F.raiseError(new RuntimeException("Unable to set immutable global"))"""}
      })"""
        }

        def makeFunction(effectful: Boolean)(sym: Symbol): Tree = atPos(sym.pos) {
          val readers =
            sym.typeSignature.paramLists match {
              case List(params) =>
                params.zipWithIndex.map {
                  case (param, idx) =>
                    val name = TermName(s"reader$idx")
                    (name,
                     q"""val $name = implicitly[_root_.swam.runtime.formats.ValueReader[$f, ${param.typeSignature}]]""")
                }
              case _ => c.abort(sym.pos, "Only functions with one parameter lists can be exported")
            }

          val hasResult =
            if (effectful)
              !(sym.asMethod.returnType.typeArgs.exists(_ <:< typeOf[Unit]))
            else
              !(sym.asMethod.returnType <:< typeOf[Unit])
          val writerName = TermName("writer")
          val writer =
            if (effectful)
              q"implicitly[_root_.swam.runtime.formats.ValueWriter[$f, ..${sym.asMethod.returnType.typeArgs}]]"
            else
              q"implicitly[_root_.swam.runtime.formats.ValueWriter[$f, ${sym.asMethod.returnType}]]"
          val values = readers.zipWithIndex.map {
            case ((name, _), idx) =>
              val vname = TermName(s"value$idx")
              (vname, fq"$vname <- $name.read(parameters($idx), m)")
          }
          val res =
            if (effectful)
              if (hasResult)
                Seq(fq"raw <- t.${sym.name.toTermName}(..${values.map(_._1)})", fq"res <- $writerName.write(raw, m)")
              else
                Seq(fq"_ <- t.${sym.name.toTermName}(..${values.map(_._1)})")
            else if (hasResult)
              Seq(fq"res <- $writerName.write(t.${sym.name.toTermName}(..${values.map(_._1)}), m)")
            else
              Seq(fq"_ <- t.${sym.name.toTermName}(..${values.map(_._1)})")

          q"""F.pure(new _root_.swam.runtime.Function[$f] {
            ..${readers.map(_._2)}
            ${if (hasResult) q"val $writerName = $writer" else q""}
            val tpe = _root_.swam.FuncType(Vector(..${readers
            .map(p => q"${p._1}.swamType")}), Vector(..${if (hasResult) q"$writerName.swamType" else q""}))
            def invoke(parameters: Vector[_root_.swam.runtime.Value], m: _root_.scala.Option[_root_.swam.runtime.Memory[$f]]): $f[_root_.scala.Option[_root_.swam.runtime.Value]] = for(
              ..${values.map(_._2)};
              ..$res
          ) yield ${if (hasResult) q"_root_.scala.Some(res)" else q"_root_.scala.None"}
          })"""
        }

        private def makeExport(mods: Modifiers, sym: Symbol, maker: Symbol => Tree, pos: Position) = {
          val ename = sym.annotations
            .collectFirst {
              case ExportedName(name) => name
            }
            .getOrElse(atPos(pos)(q"""${sym.name.decodedName.toString.trim}"""))
          Some((ename, sym, maker))
        }

        def unapply(a: Tree): Option[(Tree, Symbol, Symbol => Tree)] = a match {
          case t @ q"$mods val $tname: $tpt = $expr" if t.symbol.annotations.exists(isGlobal(_)) =>
            makeExport(mods, t.symbol, makeGlobal(false), t.pos)
          case t @ q"$mods var $tname: $tpt = $expr" if t.symbol.annotations.exists(isGlobal(_)) =>
            makeExport(mods, t.symbol, makeGlobal(true), t.pos)
          case t @ q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr"
              if t.symbol.annotations.exists(isPure(_)) =>
            makeExport(mods, t.symbol, makeFunction(false), t.pos)
          case t @ q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr"
              if t.symbol.annotations.exists(isEffectful(_)) =>
            makeExport(mods, t.symbol, makeFunction(true), t.pos)
          case t =>
            None
        }
      }

      // first start by collecting all fields marked as exported
      val ts =
        stats.collect {
          case Export(ename, name, writable) => (ename, name, writable)
        }

      val tpnames = tparams.map(_.name)

      checkNames(ts)

      val asInstanceName = c.freshName(TermName(s"${tpname}AsInstance"))

      val cases = ts.map { case (ename, name, mk) => (cq"$ename => ${mk(name)}") }

        q"""implicit def $asInstanceName[..$efftpe, ..$tparams](implicit F: _root_.cats.MonadError[$f,Throwable]): _root_.swam.runtime.imports.AsInstance[$tpname[..$tpnames], $f] = new _root_.swam.runtime.imports.AsInstance[$tpname[..$tpnames], $f] {
      import _root_.cats.implicits._
      def find(t: $tpname[..$tpnames], field: _root_.java.lang.String): $f[_root_.swam.runtime.Interface[$f, _root_.swam.Type]] =
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
