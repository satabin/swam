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

import imports._
import compiler._
import interpreter._

import cats._
import cats.implicits._

import runtime._

import scala.language.higherKinds

private[runtime] class Instantiator[F[_]](engine: SwamEngine[F])(implicit F: MonadError[F, Throwable]) {

  private val interpreter = engine.interpreter
  private val dataOnHeap = engine.conf.data.onHeap

  def instantiate[I](module: Module[F], imports: I)(implicit I: Imports[I, F]): F[Instance[F]] = {
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

  private def check[I](mimports: Vector[Import], provided: I)(implicit I: Imports[I, F]): F[Vector[Interface[F, Type]]] =
    F.tailRecM((0, Vector.empty[Interface[F, Type]])) {
      case (idx, acc) =>
        if (idx >= mimports.size) {
          F.pure(Right(acc))
        } else {
          val imp = mimports(idx)
          I.find(provided, imp.moduleName, imp.fieldName).flatMap { provided =>
            if (provided.tpe <:< imp.tpe) {
              F.pure(Left((idx + 1, acc :+ provided)))
            } else {
              F.raiseError(new RuntimeException(s"Expected import of type ${imp.tpe} but got ${provided.tpe}"))
            }
          }
        }
    }

  private def initialize(globals: Vector[CompiledGlobal],
                         imports: Vector[Interface[F, Type]]): F[Vector[GlobalInstance[F]]] = {
    val impglobals = imports.collect {
      case g: Global[F] => g
    }
    val inst = new Instance[F](null, interpreter)
    inst.globals = impglobals
    F.tailRecM((0, Vector.empty[GlobalInstance[F]])) {
      case (idx, acc) =>
        if (idx >= globals.size)
          F.pure(Right(acc))
        else
          globals(idx) match {
            case CompiledGlobal(tpe, init) =>
              interpreter.interpretInit(tpe.tpe, init, inst).map { ret =>
                val i = new GlobalInstance[F](tpe)
                i.set(ret.get)
                Left((idx + 1, acc :+ i))
              }
          }
    }
  }

  private def allocate(module: Module[F],
                       globals: Vector[GlobalInstance[F]],
                       imports: Vector[Interface[F, Type]]): F[Instance[F]] = {

    val instance = new Instance[F](module, interpreter)

    val (ifunctions, iglobals, itables, imemories) = imports.foldLeft(
      (Vector.empty[Function[F]],
       Vector.empty[Global[F]],
       Vector.empty[Table[F]],
       Vector.empty[Memory[F]])) {
      case ((ifunctions, iglobals, itables, imemories), f: Function[F]) =>
        (ifunctions :+ f, iglobals, itables, imemories)
      case ((ifunctions, iglobals, itables, imemories), g: Global[F]) =>
        (ifunctions, iglobals :+ g, itables, imemories)
      case ((ifunctions, iglobals, itables, imemories), t: Table[F]) =>
        (ifunctions, iglobals, itables :+ t, imemories)
      case ((ifunctions, iglobals, itables, imemories), m: Memory[F]) =>
        (ifunctions, iglobals, itables, imemories :+ m)
    }
    instance.funcs = ifunctions ++ module.functions.map {
      case CompiledFunction(tpe, locals, code) =>
      new FunctionInstance[F](tpe, locals, code, instance)
    }
    instance.globals = iglobals ++ globals
    instance.tables = itables ++ module.tables.map {
      case TableType(_, limits) => new TableInstance[F](limits.min, limits.max)
    }
    instance.memories = imemories ++ module.memories.map {
      case MemType(limits) => new MemoryInstance[F](limits.min, limits.max, dataOnHeap)
    }
    instance.exps = module.exports.map {
      case Export.Function(name, tpe, idx) => (name, instance.funcs(idx))
      case Export.Global(name, tpe, idx)   => (name, instance.globals(idx))
      case Export.Table(name, tpe, idx)    => (name, instance.tables(idx))
      case Export.Memory(name, tpe, idx)   => (name, instance.memories(idx))
    }.toMap
    F.pure(instance)
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
              val offset = roffset.get.asInt
              if (init.size + offset > instance.tables(0).size) {
                F.raiseError(new RuntimeException("Overflow in table initialization"))
              } else {
                for (initi <- 0 until init.size)
                  instance.tables(0)(offset + initi) = instance.funcs(init(initi))
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
              val offset = roffset.get.asInt
              println(s"${init.capacity + offset} - ${instance.memories(0).size}")
              if (init.capacity + offset > instance.memories(0).size) {
                F.raiseError(new RuntimeException("Overflow in memory initialization"))
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
