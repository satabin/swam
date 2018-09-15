import swam._
import text._
import runtime._
import cats.effect._
import java.nio.file.Paths

val tcompiler = new Compiler[IO]

val engine = SwamEngine[IO]()

val strings =
  for {
    engine <- engine
    m <- engine.compile(tcompiler.stream(Paths.get("examples/string.wat"), true))
    i <- m.newInstance()
    s1 <- {
      import formats.string.cstring
      i.exports.typed.global[String]("c-like")
    }
    s2 <- {
      import formats.string.utf8
      i.exports.typed.global[String]("utf8")
    }
  } yield (s1, s2)

val (s1, s2) = strings.unsafeRunSync()
println(s1)
println(s2)
