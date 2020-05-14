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
      q"""
       $clsDef
       object ${clsDef.name.toTermName} {
                  ${module(clsDef)}
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
                  ${module(clsDef)}
       }
       """
    case _ => c.abort(c.enclosingPosition, "Invalid annotation target: must be a class")
  }

  private def isAnnotation(ann: Tree, aname: String): Boolean = ann match {
    case Apply(Select(New(Select(_, name)), _), args) if name.decodedName.toString == aname => true
    case Apply(Select(New(Ident(name)), _), args) if name.decodedName.toString == aname     => true
    case _                                                                                  => false
  }

  private def isGlobal(ann: Tree): Boolean = isAnnotation(ann, "global")

  private def isPure(ann: Tree): Boolean = isAnnotation(ann, "pure")

  private def isEffectful(ann: Tree): Boolean = isAnnotation(ann, "effectful")

  private def isEffect(ann: Tree): Boolean = isAnnotation(ann, "effect")

  private val names = Set("global", "pure", "effectful")

  private object ExportedName {
    def unapply(ann: Tree): Option[Tree] = {
      ann match {
        case Apply(Select(New(Select(_, name)), _), args) if names.contains(name.decodedName.toString) =>
          args.collectFirst {
            case q"name = $name" => name
            case q"$name"        => name
          }
        case Apply(Select(New(Ident(name)), _), args) if names.contains(name.decodedName.toString) =>
          args.collectFirst {
            case q"name = $name" => name
            case q"$name"        => name
          }
        case _ =>
          None
      }
    }
  }

  private def module(cls: Tree): Tree = cls match {
    case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
      val (efftpe, f) = tparams
        .collectFirst {
          case TypeDef(mods, name, _, _) if mods.annotations.exists(isEffect(_)) =>
            (Seq.empty[TypeDef], name.toTypeName)
        }
        .getOrElse {
          val f = c.freshName(TypeName("F"))
          (Seq(q"type $f[_]"), f)
        }

      object Export {
        def makeGlobal(writable: Boolean)(vdd: ValOrDefDef): Tree = atPos(vdd.pos) {
          val evidence =
            if (writable)
              q"implicitly[_root_.swam.runtime.formats.SimpleValueFormatter[$f, ${vdd.tpt}]]"
            else
              q"implicitly[_root_.swam.runtime.formats.SimpleValueWriter[$f, ${vdd.tpt}]]"
          q"""F.pure(new _root_.swam.runtime.Global[$f] {
        private val formatter = $evidence
        val tpe = _root_.swam.GlobalType(formatter.swamType, ${if (writable) q"_root_.swam.Mut.Var"
          else q"_root_.swam.Mut.Const"})
        def get: _root_.swam.runtime.Value = formatter.write(t.${TermName(vdd.name.decodedName.toString.trim)})
        def set(v: _root_.swam.runtime.Value) = ${if (writable)
            q"formatter.read(v).map(t.${TermName(vdd.name.decodedName.toString.trim)} = _)"
          else
            q"""F.raiseError(new RuntimeException("Unable to set immutable global"))"""}
      })"""
        }

        def makeFunction(effectful: Boolean)(vdd: ValOrDefDef): Tree = atPos(vdd.pos) {
          val dd = vdd match {
            case dd: DefDef => dd
            case _          => c.abort(vdd.pos, "This is a bug")
          }
          val readers =
            dd.vparamss match {
              case List(params) =>
                params.zipWithIndex.map {
                  case (param, idx) =>
                    val name = TermName(s"reader$idx")
                    (name, q"""val $name = implicitly[_root_.swam.runtime.formats.ValueReader[$f, ${param.tpt}]]""")
                }
              case _ => c.abort(dd.pos, "Only functions with one parameter lists can be exported")
            }

          val writerName = TermName("writer")
          val writerType = dd.tpt match {
            case tq"$f[$t]" if effectful => t
            case t                       => t
          }
          val writer =
            q"_root_.swam.runtime.formats.ValuesWriter[$f, $writerType]"
          val values = readers.zipWithIndex.map {
            case ((name, _), idx) =>
              val vname = TermName(s"value$idx")
              (vname, fq"$vname <- $name.read(parameters($idx), m)")
          }
          val res =
            if (effectful)
              Seq(fq"raw <- t.${dd.name.toTermName}(..${values.map(_._1)})", fq"res <- $writerName.write(raw, m)")
            else
              Seq(fq"res <- $writerName.write(t.${dd.name.toTermName}(..${values.map(_._1)}), m)")

          q"""F.pure(new _root_.swam.runtime.Function[$f] {
            ..${readers.map(_._2)}
            val $writerName = $writer
            val tpe = _root_.swam.FuncType(Vector(..${readers
            .map(p => q"${p._1}.swamType")}), $writerName.swamTypes)
            def invoke(parameters: Vector[_root_.swam.runtime.Value], m: _root_.scala.Option[_root_.swam.runtime.Memory[$f]]): $f[_root_.scala.Vector[_root_.swam.runtime.Value]] = for(
              ..${values.map(_._2)};
              ..$res
          ) yield res
          })"""
        }

        private def makeExport(t: ValOrDefDef, maker: ValOrDefDef => Tree, pos: Position) = {
          val ename = t.mods.annotations
            .collectFirst {
              case ExportedName(name) => name
            }
            .getOrElse(atPos(pos)(q"""${t.name.decodedName.toString.trim}"""))
          Some((ename, t, maker))
        }

        def unapply(a: Tree): Option[(Tree, ValOrDefDef, ValOrDefDef => Tree)] = a match {
          case vd @ ValDef(mods, tname, tpt, expr) if mods.annotations.exists(isGlobal(_)) =>
            checkReturnType(vd.pos, tpt)
            makeExport(vd, makeGlobal(mods.hasFlag(Flag.MUTABLE)), vd.pos)
          case dd @ DefDef(mods, tname, tparams, params, tpt, expr) if mods.annotations.exists(isPure(_)) =>
            checkReturnType(dd.pos, tpt)
            makeExport(dd, makeFunction(false), dd.pos)
          case dd @ DefDef(mods, tname, tparams, params, tpt, expr) if mods.annotations.exists(isEffectful(_)) =>
            checkReturnType(dd.pos, tpt)
            makeExport(dd, makeFunction(true), dd.pos)
          case t =>
            None
        }
      }

      // first start by collecting all fields marked as exported
      val ts =
        stats.collect {
          case Export(ename, vdd, writable) => (ename, vdd, writable)
        }

      val tpnames = tparams.map(_.name)

      checkNames(ts)

      val asInstanceName = c.freshName(TermName(s"${tpname}AsInstance"))

      val cases = ts.map { case (ename, vdd, mk) => (cq"$ename => ${mk(vdd)}") }

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

  private def checkReturnType(pos: Position, tpt: Tree): Unit =
    tpt match {
      case tq"" =>
        c.abort(pos, "Exported declarations must have a type annotation")
      case _ =>
      // ok
    }

  private def checkNames(ts: List[(Tree, ValOrDefDef, ValOrDefDef => Tree)]): Unit = {
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
