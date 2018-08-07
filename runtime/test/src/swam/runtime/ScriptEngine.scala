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
import imports._
import unresolved._

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
  def requireLast(pos: Int): IO[Instance[IO]] =
    last match {
      case Some(i) => IO.pure(i)
      case None    => IO.raiseError(new ScriptException(s"No last defined module to register", pos))
    }

  def module(pos: Int, id: Option[String]): IO[Instance[IO]] =
    id match {
      case Some(id) =>
        modules.get(id) match {
          case Some(i) => IO.pure(i)
          case None    => IO.raiseError(new ScriptException(s"Unknown module $id", pos))
        }
      case None => requireLast(pos)
    }

}

class ScriptEngine {

  val IO = implicitly[Effect[IO]]

  val engine = new SwamEngine[IO]

  val tcompiler = new Compiler[IO]

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
          case Register(name, modid) =>
            // register the given instance with import name
            for (mod <- ctx.module(cmd.pos, modid))
              yield Left((rest, ctx.copy(imports = ctx.imports.updated(name, mod))))
          case Get(modid, name) =>
            for (_ <- get(ctx, cmd.pos, modid, name))
              yield Left((rest, ctx))
          case Invoke(modid, export, params) =>
            for (_ <- invoke(ctx, cmd.pos, modid, export, params))
              yield Left((rest, ctx))
          case AssertReturn(action, result) =>
            for {
              actual <- execute(ctx, action)
              expected <- value(cmd.pos, result.headOption)
              _ <- check(cmd.pos, actual, expected)
            } yield Left((rest, ctx))
          case _ =>
            // ignore other commands
            IO.pure(Left((rest, ctx)))
        }
      case (Seq(), _) =>
        IO.pure(Right(()))
    }

  def check(pos: Int, actual: Option[Value], expected: Option[Value]): IO[Unit] =
    if (actual == expected)
      IO.pure(())
    else
      IO.raiseError(new ScriptException(s"Expected $actual but got $expected", pos))

  def value(pos: Int, i: Option[Inst]): IO[Option[Value]] =
    i match {
      case Some(Constant(v)) => IO.pure(Some(v))
      case Some(_)           => IO.raiseError(new ScriptException(s"Expected constant but got $i", pos))
      case None              => IO.pure(None)
    }

  def value(pos: Int, i: Inst): IO[Value] =
    i match {
      case Constant(v) => IO.pure(v)
      case _           => IO.raiseError(new ScriptException(s"Expected constant but got $i", pos))
    }

  def execute(ctx: ExecutionContext, action: Action): IO[Option[Value]] =
    action match {
      case Invoke(modid, name, params) => invoke(ctx, action.pos, modid, name, params)
      case Get(modid, name)            => get(ctx, action.pos, modid, name).map(Some(_))
    }

  def get(ctx: ExecutionContext, pos: Int, modid: Option[String], name: String): IO[Value] =
    for {
      mod <- ctx.module(pos, modid)
      g <- mod.exports.global(name)
    } yield g.get

  def invoke(ctx: ExecutionContext, pos: Int, modid: Option[String], export: String, params: Expr): IO[Option[Value]] = {
    val values =
      IO.tailRecM((params, Seq.empty[Value])) {
        case (Seq(), acc)             => IO.pure(Right(acc))
        case (Seq(p, rest @ _*), acc) => value(pos, p).map(v => Left((rest, acc :+ v)))
      }
    for {
      i <- ctx.module(pos, modid)
      ps <- values
      f <- i.exports.function(export)
      res <- f.invoke(ps.toVector)
    } yield res
  }

}
