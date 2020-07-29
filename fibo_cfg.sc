import swam.text._
import swam.cfg._
import swam.runtime._
import swam.binary._

import swam.validation._

import cats.effect._

import java.nio.file.Paths
import java.io._
import fs2._

import cats.effect.{Blocker, ContextShift, IO}
import scala.concurrent.ExecutionContext


implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)


def GetCFG(func_id : Int, wat : Boolean, fileName: String) : CFG = {
  val cfg =
  Blocker[IO].use { blocker =>
    for {
      engine <- Engine[IO](blocker)
      validator <- Validator[IO](blocker)
      binaryParser = new ModuleParser[IO](validator)
      compiler <- Compiler[IO](blocker)
      module = 
        if (wat) compiler.stream(Paths.get(fileName), false, blocker) 
        else engine.sections(Paths.get(fileName), blocker)
      mod <- binaryParser.parse(module)
      cfg = mod.funcs(func_id)
      //naive <- compiler.compile(Paths.get("fibo.wat"), blocker).map(_.funcs(0))
      cfg <- CFGicator.buildCFG[IO](cfg.body)
    } yield cfg
  }.unsafeRunSync()
  cfg
}

val cfg0 = GetCFG(0, false, "check-for.wasm")
val cfg1 = GetCFG(1, false, "check-for.wasm")
val cfg2 = GetCFG(2, false, "check-for.wasm")
val cfg3 = GetCFG(3, false, "check-for.wasm")
println("This is cfg for func 0" + cfg0.blocks)
println("This is cfg for func 1" + cfg1.blocks)
println("This is cfg for func 2" + cfg2.blocks) //--- Issues getting exception here
println("This is cfg for func 3" + cfg3.blocks)
