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
package internals
package instance

import interpreter._

import cats._
import cats.implicits._

import runtime._

import scala.language.higherKinds

private[runtime] class Instantiator[F[_]](interpreter: Interpreter[F])(implicit F: MonadError[F, Throwable]) {

  def instantiate(module: Module[F], imports: Imports[F]): F[Instance[F]] = {
    for {
      // check and order the imports
      imports <- check(module.imports, imports)
      // now initialize the globals
      globals <- initialize(module.globals, imports)
      // allocate the module
      instance <- allocate(module, globals, imports)
      // we now have a fresh instance with imports and exports all wired, and various memory areas allocated, time to initialize tables
      _ <- initTables(instance)
      // now initialize memories
      _ <- initMems(instance)
      // and finally start the instance
      _ <- start(instance)
    } yield instance
  }

  private def check(mimports: Vector[Import], provided: Imports[F]): F[Vector[ImportableInstance[F]]] =
    F.tailRecM((0, Vector.empty[ImportableInstance[F]])) {
      case (idx, acc) =>
        if (idx >= mimports.size) {
          F.pure(Right(acc))
        } else {
          val imp = mimports(idx)
          provided.findField(imp.moduleName, imp.fieldName).flatMap { provided =>
            if (provided.tpe <:< imp.tpe) {
              F.pure(Left((idx + 1, acc :+ provided)))
            } else {
              F.raiseError(new RuntimeException(s"Expected import of type ${imp.tpe} but got ${provided.tpe}"))
            }
          }
        }
    }

  private def initialize(globals: Vector[CompiledGlobal],
                         imports: Vector[ImportableInstance[F]]): F[Vector[GlobalInstance[F]]] = {
    val impglobals = imports.collect {
      case g: GlobalInstance[F] => g
    }
    val inst = new Instance[F](null, Map.empty, interpreter, impglobals, Vector.empty, Vector.empty, Vector.empty)
    F.tailRecM((0, Vector.empty[GlobalInstance[F]])) {
      case (idx, acc) =>
        if (idx >= globals.size)
          F.pure(Right(acc))
        else
          globals(idx) match {
            case InterpretedCompiledGlobal(tpe, init) =>
              interpreter.interpretInit(tpe.tpe, init, inst).map { ret =>
                val i = new InterpretedGlobalInstance[F](tpe)
                i.value = ret(0)
                Left((idx + 1, acc :+ i))
              }
            case _ =>
              F.pure(Left((idx + 1, acc)))
          }
    }
  }

  private def allocate(module: Module[F],
                       globals: Vector[GlobalInstance[F]],
                       imports: Vector[ImportableInstance[F]]): F[Instance[F]] = {
    val (ifunctions, iglobals, itables, imemories) = imports.foldLeft(
      (Vector.empty[CompiledFunction[F]],
       Vector.empty[GlobalInstance[F]],
       Vector.empty[TableInstance[F]],
       Vector.empty[MemoryInstance])) {
      case ((ifunctions, iglobals, itables, imemories), ExportedField.Function(_, inst)) =>
        (ifunctions :+ inst, iglobals, itables, imemories)
      case ((ifunctions, iglobals, itables, imemories), ExportedField.Global(_, inst)) =>
        (ifunctions, iglobals :+ inst, itables, imemories)
      case ((ifunctions, iglobals, itables, imemories), ExportedField.Table(_, inst)) =>
        (ifunctions, iglobals, itables :+ inst, imemories)
      case ((ifunctions, iglobals, itables, imemories), ExportedField.Memory(_, inst)) =>
        (ifunctions, iglobals, itables, imemories :+ inst)
    }
    val finstances = ifunctions ++ module.functions
    val ginstances = iglobals ++ globals
    val tinstances = itables ++ module.tables.map {
      case TableType(_, limits) => new TableInstance[F](limits.min, limits.max)
    }
    val minstances = imemories ++ module.memories.map {
      case MemType(limits) => new MemoryInstance(limits.min, limits.max)
    }
    val exported = module.exports.map {
      case Export.Function(name, tpe, idx) => (name, ExportedField.Function[F](tpe, finstances(idx)))
      case Export.Global(name, tpe, idx)   => (name, ExportedField.Global[F](tpe, ginstances(idx)))
      case Export.Table(name, tpe, idx)    => (name, ExportedField.Table[F](tpe, tinstances(idx)))
      case Export.Memory(name, tpe, idx)   => (name, ExportedField.Memory[F](tpe, minstances(idx)))
    }
    F.pure(new Instance[F](module, exported.toMap, interpreter, ginstances, minstances, finstances, tinstances))
  }

  private def initTables(instance: Instance[F]): F[Unit] = {
    val module = instance.module
    F.tailRecM(0) { idx =>
      if (idx >= module.elems.size)
        F.pure(Right(()))
      else
        module.elems(idx) match {
          case CompiledElem(coffset, init) =>
            interpreter.interpretInit(ValType.I32, coffset, instance).flatMap { roffset =>
              val offset = roffset(0).asInt
              if (init.size + offset >= instance.tables(0).size) {
                F.raiseError(new RuntimeException("Overflow in table initialization"))
              } else {
                for (initi <- 0 until init.size)
                  instance.tables(0)(offset + initi) = init(initi)
                F.pure(Left(idx + 1))
              }
            }
        }
    }
  }

  private def initMems(instance: Instance[F]): F[Unit] = {
    val module = instance.module
    F.tailRecM(0) { idx =>
      if (idx >= module.data.size)
        F.pure(Right(()))
      else
        module.data(idx) match {
          case CompiledData(coffset, init) =>
            interpreter.interpretInit(ValType.I32, coffset, instance).flatMap { roffset =>
              val offset = roffset(0).asInt
              if (init.capacity + offset >= instance.memories(0).size) {
                F.raiseError(new RuntimeException("Overflow in table initialization"))
              } else {
                instance.memories(0).writeBytes(offset, init)
                F.pure(Left(idx + 1))
              }
            }
        }
    }
  }

  private def start(instance: Instance[F]): F[Unit] =
    instance.module.start match {
      case Some(start) => interpreter.interpret(start, Vector(), instance).map(_ => ())
      case None        => F.pure(())
    }

}
