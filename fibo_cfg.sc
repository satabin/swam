import swam.text._
import swam.cfg._
import swam.runtime._
import swam.binary._

import swam.validation._

import cats.effect._

import java.nio.file.Paths
import java.io._
import fs2._


implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

val wat = true

//val wastPositioner = new WastPositioner(Paths.get("fibo.wat"))

val c =
    Blocker[IO].use { blocker => for {
        engine <- Engine[IO](blocker)
        validator <- Validator[IO](blocker)
        binaryParser = new ModuleParser[IO](validator)
        tcompiler <- swam.text.Compiler[IO](blocker)
        module = if (wat) tcompiler.stream(Paths.get("fibo.wat"), false, blocker) else engine.sections(Paths.get("Babbage_problem.wasm"), blocker)
        mod <- binaryParser.parse(module)
        //cfg <- CFGicator.buildCFG[IO](mod)
        //cfg <- CFGicator.buildCFG[IO](naive.body)
      }yield mod
    }.unsafeRunSync()

println(c)

//for(x <- 0 to 2){
  //cfgs.funcs(x).body
  //var cfg = CFGicator.buildCFG[IO](cfgs.funcs(x).body)
  //val program: IO[swam.cfg.CFG] = 
   // for {
   //    s1 <- cfg
   // } yield s1
  //  program.unsafeRunSync()
  //println(program)
//}



/*var count = 0

def getModule(fileName: String) = {
  val module1 = 
    Blocker[IO].use { blocker => for {
        engine <- Engine[IO](blocker)
        validator <- Validator[IO](blocker)
        binaryParser = new ModuleParser[IO](validator)
        tcompiler <- swam.text.Compiler[IO](blocker)
        module = if (wat) tcompiler.stream(Paths.get(fileName), false, blocker) else engine.sections(Paths.get(fileName), blocker)
        //mod <- binaryParser.parse(module).map(_.funcs().body)
        
        mod <- binaryParser.parse(module)

        //cfg <- CFGicator.buildCFG[IO](mod)
        //cfg <- CFGicator.buildCFG[IO](naive.body)
      }yield mod
    }.unsafeRunSync()

  module1
}

//val r = getModule("wasm_programs/3.wasm")

//val r = getModule("Babbage_problem.wasm")
val r = getModule("Deconvolution-1D.wasm")

def getCFGForFunc(func_id:Int,module: swam.syntax.Module) = {
  println("Thsi is funcs at 5" + module.funcs(5))
  val cfgs = module.funcs.map(x=>{  
        CFGicator.buildCFG[IO](x.body)
      })
  //println(cfgs(0))
  val program: IO[swam.cfg.CFG] = 
    for {
       cfg <- cfgs(func_id)
    } yield cfg
    program.unsafeRunSync()
}

var result  = getCFGForFunc(5,r)
println(s"This is a cfg for method 5 : " + result.blocks)
*/
/**for(x <- 0 to 4){
   //println(x+1)
   var result  = getCFGForFunc(x,r)
   println(s"This is a cfg for method $x : " + result.blocks)
   //println(x)
}

for(x <- 6 to 39){
   //println(x+1)
   var result  = getCFGForFunc(x,r)
   println(s"This is a cfg for method $x : " + result.blocks)
   //println(x)
}

for(x <- 42 to 50){
   //println(x+1)
   var result  = getCFGForFunc(x,r)
   println(s"This is a cfg for method $x : " + result.blocks)
   //println(x)
}*/

