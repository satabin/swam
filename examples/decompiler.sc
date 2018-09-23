import swam._
import text._
import decompilation._
import util.pretty._
import cats.effect._
import java.nio.file.Paths

val tcompiler = new Compiler[IO]

val rdecompiler = new RawDecompiler[IO]

val tdecompiler = new TextDecompiler[IO]

def compdec(p: String): (Doc, Doc) =
  (for {
    rd <- rdecompiler.decompile(tcompiler.stream(Paths.get(p), true))
    td <- tdecompiler.decompile(tcompiler.stream(Paths.get(p), true))
  } yield (rd, td)).unsafeRunSync()

val (rd, td) = compdec("examples/fibo.wat")

println("********* Raw decompilation")
println(rd.render(0))
println()
println("********* Smart decompilation")
println(td.render(0))
