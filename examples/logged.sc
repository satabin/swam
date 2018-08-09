import swam._
import swam.text._
import swam.runtime._
import swam.runtime.imports._
import swam.runtime.formats.DefaultFormatters._
import cats.effect._
import java.nio.file.Paths


val tcompiler = new Compiler[IO]

val engine = new SwamEngine[IO]

def log(i: Int) = IO(println(s"got $i"))

type AsIIO[T] = AsInterface[T, IO]

val foo = Imports[IO](module("console", TCMap[String, AsIIO]("log" -> log _)))

val f = (for {
  mod <- engine.compile(tcompiler.stream(Paths.get("examples/logged.wat"), true))
  inst <- mod.newInstance(foo)
  f <- inst.exports.typed.function1[Int, Int]("add42")
} yield f).unsafeRunSync()

println(f(4).unsafeRunSync())
