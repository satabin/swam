import swam.text._
import cats.effect._
import swam.cfg._
import swam.runtime._ 
import swam.validation._
import swam.binary._
import java.nio.file.Paths

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

for(x <- 0 to 59)
{
  val cfg = GetCFG(x, false, "Largest_number_divisible_by_its_digits.wasm")  
  println(s"This is cfg for func $x :: ${cfg.blocks}")
}
