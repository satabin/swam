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

import text._
import test._
import syntax._
import imports._

import cats._
import cats.implicits._
import cats.effect._

object Constant {
  def unapply(i: Inst): Option[Value] =
    i match {
      case i32.Const(v) => Some(Value.Int32(v))
      case i64.Const(v) => Some(Value.Int64(v))
      case f32.Const(v) => Some(Value.Float32(v))
      case f64.Const(v) => Some(Value.Float64(v))
      case _            => None
    }
}

case class ExecutionContext(imports: Imports[IO], modules: Map[String, Instance[IO]], last: Option[Instance[IO]]) {
  def requireLast: IO[Instance[IO]] =
    last match {
      case Some(i) => IO.pure(i)
      case None    => IO.raiseError(new Exception(s"No last defined module to register"))
    }

}

class ScriptEngine {

  val IO = implicitly[Effect[IO]]

  val engine = new SwamEngine[IO]

  val tcompiler = new Compiler[IO]

  def requireLast(last: Option[Instance[IO]]): IO[Instance[IO]] =
    last match {
      case Some(i) => IO.pure(i)
      case None    => IO.raiseError(new Exception(s"No last defined module to register"))
    }

  def run(commands: Seq[Command]): IO[Unit] =
    IO.tailRecM((commands, ExecutionContext(spectestlib, Map.empty, None))) {
      case (Seq(cmd, rest @ _*), ctx) =>
        cmd match {
          case ValidModule(mod) =>
            for {
              compiled <- engine.compile(tcompiler.stream(mod, true))
              instance <- compiled.newInstance(ctx.imports)
            } yield
              compiled.name match {
                case Some(name) =>
                  Left((rest, ctx.copy(modules = ctx.modules.updated(name, instance), last = Some(instance))))
                case None => Left((rest, ctx.copy(last = Some(instance))))
              }
          case Register(name, Some(id)) =>
            // register the given instance with import name
            IO.pure(Left((rest, ctx.copy(imports = ctx.imports.updated(name, ctx.modules(id))))))
          case Register(name, None) =>
            for (i <- ctx.requireLast)
              yield Left((rest, ctx.copy(imports = ctx.imports.updated(name, i))))
          case Invoke(modid, export, params) =>
            invoke(ctx, modid, export, params).map(_ => Left((rest, ctx)))
        }
      case (Seq(), _) =>
        IO.pure(Right(()))
    }

  def invoke(ctx: ExecutionContext,
             modid: Option[String],
             export: String,
             params: unresolved.Expr): IO[Option[Value]] = {
    val mod = modid match {
      case Some(id) => IO.pure(ctx.modules(id))
      case None     => ctx.requireLast
    }
    val values =
      IO.tailRecM((params, Seq.empty[Value])) {
        case (Seq(), acc)                       => IO.pure(Right(acc))
        case (Seq(Constant(v), rest @ _*), acc) => IO.pure(Left((rest, acc :+ v)))
        case (Seq(p, _*), _)                    => IO.raiseError(new Exception(s"Expected constant but got $p"))
      }
    for {
      i <- mod
      ps <- values
      f <- i.exports.function(export)
      res <- f.invoke(ps.toVector)
    } yield res
  }

}
