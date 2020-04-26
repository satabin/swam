---
title: Strings
---

WebAssembly only has value integer types. However, providing a correct reader, one can interpret an integer as the address of a complex object in memory and convert it back to scala when exported. By default, Swam provides such readers for strings, either C-like or UTF-8 encoded strings. Let’s assume we have [a WebAssembly module that exports two strings](/examples/string.wat)

```scala mdoc:silent
import swam._
import text._
import runtime._
import cats.effect._
import java.nio.file.Paths

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

val strings =
  Blocker[IO].use { blocker =>
    for {
      engine <- Engine[IO](blocker)
      tcompiler <- Compiler[IO](blocker)
      m <- engine.compile(tcompiler.stream(Paths.get("string.wat"), true, blocker))
      i <- m.instantiate
      s1 <- {
        import formats.string.cstring
        i.exports.typed.global[String]("c-like")
      }
      s2 <- {
        import formats.string.utf8
        i.exports.typed.global[String]("utf8")
      }
    } yield (s1, s2)
  }

val (s1, s2) = strings.unsafeRunSync()
```

Reading the exported strings results in:
```scala mdoc
println(s1)
println(s2)
```
