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
import trace._
import cats.effect._
import cats.implicits._
import swam.binary.custom.FunctionNames
import swam.cfg.CFGicator
import swam.runtime.internals.interpreter.{AsmInst, InstructionListener, InstructionWrapper}

private[runtime] class Instantiator[F[_]](engine: Engine[F])(implicit F: Async[F]) {

  private val interpreter = engine.interpreter
  private val dataOnHeap = engine.conf.data.onHeap
  private val dataHardMax = engine.conf.data.hardMax
  private val random = scala.util.Random

  def instantiate(module: Module[F], imports: Imports[F]): F[Instance[F]] = {
    for {
      // check and order the imports
      imports <- check(module.imports, imports)
      // now initialize the globals
      globals <- initialize(module.globals, imports)
      // allocate the module
      instance <- allocate(module, globals, imports)
      // we now have a fresh instance with imports and exports all wired, and various memory areas allocated, time to initialize and start it
      _ <- instance.init
    } yield instance
  }

  private def check(mimports: Vector[Import], provided: Imports[F]): F[Vector[Interface[F, Type]]] =
    F.tailRecM((0, Vector.empty[Interface[F, Type]])) {
      case (idx, acc) =>
        if (idx >= mimports.size) {
          F.pure(Right(acc))
        } else {
          val imp = mimports(idx)
          provided.find(imp.moduleName, imp.fieldName).flatMap { provided =>
            if (provided.tpe <:< imp.tpe)
              F.pure(Left((idx + 1, acc :+ provided)))
            else
              F.raiseError(new LinkException(s"Expected import of type ${imp.tpe} but got ${provided.tpe}"))
          }
        }
    }

  private def initialize(globals: Vector[CompiledGlobal[F]],
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
              interpreter
                .interpretInit(tpe.tpe, init, inst)
                .flatMap[Long] {
                  case Vector(res) => F.pure(res)
                  case res =>
                    F.raiseError(
                      new LinkException(s"Global expression must return a single result but got ${res.size}"))
                }
                .flatMap { res =>
                  val i = new GlobalInstance[F](tpe)
                  i.rawset(res)
                  F.pure(Left((idx + 1, acc :+ i)))
                }
          }
    }
  }

  private def getBBLeaders(code: Array[AsmInst[F]]): Set[Int] = {

    var leaders = Set[Int]()
    leaders += 0 // first instruction is a leader

    def getBBSaux(idx: Int): Unit = {

      if (idx < code.length) {

        val inst: AsmInst[F] = code(idx)

        inst match {
          // Every jump instruction target and the the next instruction is then a leader
          case x: engine.asm.Jump => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: engine.asm.JumpIf => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: engine.asm.Br => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: engine.asm.BrIf => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: engine.asm.BrLabel => {
            leaders += x.addr
            leaders += idx + 1
          }
          case x: engine.asm.BrTable => {
            x.lbls.foreach(l => {
              leaders += l.addr
            })

            leaders += x.dflt.addr
            leaders += idx + 1
          }
          case _ => {}
        }
        getBBSaux(idx + 1)
      }
    }

    getBBSaux(1)
    leaders
  }

  private def allocate(module: Module[F],
                       globals: Vector[GlobalInstance[F]],
                       imports: Vector[Interface[F, Type]]): F[Instance[F]] = {

    val instance = new Instance[F](module, interpreter)

    val (ifunctions, iglobals, itables, imemories) = imports.foldLeft(
      (Vector.empty[Function[F]], Vector.empty[Global[F]], Vector.empty[Table[F]], Vector.empty[Memory[F]])) {
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
      case CompiledFunction(typeIndex, tpe, locals, code) =>
        val functionName =
          module.names.flatMap(_.subsections.collectFirstSome {
            case FunctionNames(names) =>
              names.get(typeIndex)
            case _ =>
              None
          })

        val leaders = getBBLeaders(code)

        val toWrap = engine.instructionListener match {
          case Some(listener) => {
            // TODO change functionName to some kind of "debugging" class
            val name = functionName match {
              case Some(x) => functionName
              case _       => Option(s"f$typeIndex")
            }

            code.zipWithIndex.map {
              case (inst, i) =>
                if (leaders.contains(i))
                  new InstructionWrapper[F](random.nextInt(Int.MaxValue), inst, listener, name).asInstanceOf[AsmInst[F]]
                else inst
            }
          }
          case None => code
        }
        new FunctionInstance[F](tpe, locals, toWrap, instance, functionName)
    }
    instance.globals = iglobals ++ globals
    instance.tables = itables ++ module.tables.map {
      case TableType(_, limits) => new TableInstance[F](limits.min, limits.max)
    }
    instance.memories = imemories ++ module.memories.map {
      case MemType(limits) => new MemoryInstance[F](limits.min, limits.max, dataOnHeap, dataHardMax.bytes.toInt)
    }
    // trace memory acceses if tracer exists
    engine.tracer.foreach { tracer => instance.memories = instance.memories.map(TracingMemory(_, tracer)) }
    instance.exps = module.exports.map {
      case Export.Function(name, tpe, idx) => (name, instance.funcs(idx))
      case Export.Global(name, tpe, idx)   => (name, instance.globals(idx))
      case Export.Table(name, tpe, idx)    => (name, instance.tables(idx))
      case Export.Memory(name, tpe, idx)   => (name, instance.memories(idx))
    }.toMap
    F.pure(instance)
  }

}
