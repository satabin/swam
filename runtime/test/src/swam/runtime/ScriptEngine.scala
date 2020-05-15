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

import util._
import text._
import test._
import imports._
import unresolved._

import cats._
import cats.implicits._
import cats.effect._

import fs2._

import java.lang.{Float => JFloat, Double => JDouble}

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

class ScriptEngine(blocker: Blocker)(implicit cs: ContextShift[IO]) {

  import ScriptEngine._

  val engine = Engine[IO](blocker)

  val tcompiler = Compiler[IO](blocker)

  def run(commands: Seq[Command], positioner: Positioner[TextFilePosition]): IO[Unit] =
    Effect[IO].tailRecM((commands, ExecutionContext(spectestlib, Map.empty, None))) {
      case (Seq(cmd, rest @ _*), ctx) =>
        val res = cmd match {
          case ValidModule(mod) =>
            for {
              engine <- engine
              tcompiler <- tcompiler
              compiled <- engine.compile(tcompiler.stream(mod, true))
              instance <- compiled.importing(ctx.imports).instantiate
            } yield compiled.name match {
              case Some(name) =>
                Left((rest, ctx.copy(modules = ctx.modules.updated(name, instance), last = Some(instance))))
              case None => Left((rest, ctx.copy(last = Some(instance))))
            }
          case BinaryModule(id, bs) =>
            for {
              engine <- engine
              tcompiler <- tcompiler
              compiled <- engine.compileBytes(Stream.emits(bs.toArray))
              instance <- compiled.importing(ctx.imports).instantiate
            } yield id match {
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
              expected <- value(cmd.pos, result)
              _ <- check(cmd.pos, actual, expected)
            } yield Left((rest, ctx))
          case AssertReturnCanonicalNaN(action) =>
            for {
              actual <- execute(ctx, action)
              _ <- checkNaN(cmd.pos, actual)
            } yield Left((rest, ctx))
          case AssertReturnArithmeticNaN(action) =>
            for {
              actual <- execute(ctx, action)
              _ <- checkNaN(cmd.pos, actual)
            } yield Left((rest, ctx))
          case AssertTrap(action, failure) =>
            (execute(ctx, action) >> IO.raiseError(new Exception("A trap was expected"))).recoverWith {
              case i: TrapException =>
                if (i.getMessage.startsWith(failure))
                  IO.pure(Left((rest, ctx)))
                else
                  IO.raiseError(i)
            }
          case AssertModuleTrap(m, failure) =>
            (instantiate(ctx, m) >> IO.raiseError(new Exception("A trap was expected"))).recoverWith {
              case e: TrapException =>
                if (e.getMessage.startsWith(failure))
                  IO.pure(Left((rest, ctx)))
                else
                  IO.raiseError(e)
            }
          case AssertExhaustion(action, failure) =>
            (execute(ctx, action) >> IO.raiseError(new Exception("A trap was expected"))).recoverWith {
              case i: StackOverflowException =>
                if (i.getMessage.startsWith(failure))
                  IO.pure(Left((rest, ctx)))
                else
                  IO.raiseError(i)
            }
          case AssertMalformed(m @ BinaryModule(_, _), failure) =>
            (instantiate(ctx, m) >> IO.raiseError(new Exception("An exception was expected"))).recoverWith {
              case _: CompileException =>
                IO.pure(Left((rest, ctx)))
            }
          case AssertInvalid(m, failure) =>
            (instantiate(ctx, m) >> IO.raiseError(new Exception("An exception was expected"))).recoverWith {
              case _: CompileException =>
                IO.pure(Left((rest, ctx)))
            }
          case AssertUnlinkable(m, failure) =>
            (instantiate(ctx, m) >> IO.raiseError(new Exception("An exception was expected"))).recoverWith {
              case _: LinkException =>
                IO.pure(Left((rest, ctx)))
            }
          case _ =>
            // ignore other commands
            IO.pure(Left((rest, ctx)))
        }
        res
          .adaptError {
            case e =>
              println(positioner.render(cmd.pos))
              e
          }
      case (Seq(), _) =>
        IO.pure(Right(()))
    }

  def check(pos: Int, actual: Vector[Value], expected: Vector[Value]): IO[Unit] =
    cats.effect.IO(utest.assert(actual === expected))

  def checkNaN(pos: Int, actual: Vector[Value]): IO[Unit] =
    actual.foldLeftM(()) {
      case (_, Value.Float32(v)) => IO(utest.assert(v.isNaN))
      case (_, Value.Float64(v)) => IO(utest.assert(v.isNaN))
      case (_, _)                => IO(utest.assert(false))
    }

  def value(pos: Int, is: Vector[Inst]): IO[Vector[Value]] = {
    val consts = is.collect {
      case Constant(v) => v
    }
    if (consts.size == is.size)
      IO.pure(consts)
    else
      IO.raiseError(new ScriptException(s"Expected constants but got $is", pos))
  }

  def value(pos: Int, is: Seq[Inst]): IO[Vector[Value]] = {
    val consts = is.collect {
      case Constant(v) => v
    }.toVector
    if (consts.size == is.size)
      IO.pure(consts)
    else
      IO.raiseError(new ScriptException(s"Expected constants but got $is", pos))
  }

  def execute(ctx: ExecutionContext, action: Action): IO[Vector[Value]] =
    action match {
      case Invoke(modid, name, params) => invoke(ctx, action.pos, modid, name, params)
      case Get(modid, name)            => get(ctx, action.pos, modid, name).map(Vector(_))
    }

  def get(ctx: ExecutionContext, pos: Int, modid: Option[String], name: String): IO[Value] =
    for {
      mod <- ctx.module(pos, modid)
      g <- mod.exports.global(name)
    } yield g.get

  def invoke(ctx: ExecutionContext,
             pos: Int,
             modid: Option[String],
             export: String,
             params: Expr): IO[Vector[Value]] = {
    val values = value(pos, params)
    for {
      i <- ctx.module(pos, modid)
      ps <- values
      f <- i.exports.function(export)
      res <- f.invoke(ps.toVector, None)
    } yield res
  }

  def instantiate(ctx: ExecutionContext, m: TestModule): IO[Instance[IO]] = {
    val mod =
      m match {
        case ValidModule(m) =>
          for {
            engine <- engine
            tcompiler <- tcompiler
            res <- engine.compile(tcompiler.stream(m, false))
          } yield res
        case BinaryModule(_, bs) =>
          for {
            engine <- engine
            tcompiler <- tcompiler
            res <- engine.compileBytes(Stream.emits(bs.toArray))
          } yield res
        case QuotedModule(_, src) =>
          for {
            engine <- engine
            tcompiler <- tcompiler
            unresolved <- tcompiler.parse(src)
            m <- engine.compile(tcompiler.stream(unresolved, false))
          } yield m
      }
    mod.flatMap(_.importing(ctx.imports).instantiate)
  }

}

object ScriptEngine {

  implicit def valueEq[V <: Value]: Eq[V] = new Eq[V] {
    def eqv(v1: V, v2: V): Boolean =
      (v1, v2) match {
        case (Value.Int32(v1), Value.Int32(v2))     => v1 == v2
        case (Value.Int64(v1), Value.Int64(v2))     => v1 == v2
        case (Value.Float32(v1), Value.Float32(v2)) => JFloat.floatToRawIntBits(v1) == JFloat.floatToRawIntBits(v2)
        case (Value.Float64(v1), Value.Float64(v2)) =>
          JDouble.doubleToRawLongBits(v1) == JDouble.doubleToRawLongBits(v2)
        case _ => false
      }
  }

}
