import swam.text.{WastPositioner}
import swam.decompilation._
import cats.effect._

import java.nio.file.Paths

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

//val  wast = new swam.text.WastPositioner(Paths.get("fibo.wat"))
//Deconvolution-1D
//Convert wasm to wast and then get the lines

//Convert wasm to wast and print

val decom = Blocker[IO].use { blocker =>
for{  
    decompile <- RawDecompiler[IO] 
    doc <- decompile.decompilePath(Paths.get("Deconvolution-1D.wasm"), blocker)
  }yield doc
}

val doc = decom.unsafeRunSync()

println(doc.render(10))

val wast  = new swam.text.WastCustomPositioner(doc.render(10), Paths.get("Deconvolution-1D.wasm"))
//val lines = wast.getLines()
println(wast.getLines())
//var line = 0
//println(lines.map(x => {
//  line = line + 1
//  println(s"This is line $line : $x")
//  }
//))

//val wasm  = new swam.text.WastPositioner(Paths.get("fibo.wat"))
//val lines = wast.getLines()
//var line = 0
//println(lines.map(x => {
//  line = line + 1
//  println(s"This is line $line : $x")
//  }
//))




/*def GetCFG(func_id : Int, wat : Boolean, fileName: String) : CFG = {
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
println("This is cfg for func 2" + cfg2.blocks)
println("This is cfg for func 3" + cfg3.blocks) //--- Issues getting exception here*/
