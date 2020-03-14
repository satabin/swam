---
title: Decompilers
---

Decompilers make it possible to have human readyble representations of a compiled WebAssembly module. For instance, having defined [functions to compute fibonacci numbers](/examples/fibo.wat).

```scala mdoc:silent
import swam._
import text._
import decompilation._
import util.pretty._
import cats.effect._
import java.nio.file.Paths

val tcompiler = Compiler[IO]

val rdecompiler = RawDecompiler[IO]

val tdecompiler = TextDecompiler[IO]

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

def compdec(p: String): (Doc, Doc) =
  Blocker[IO].use { blocker =>
    for {
      tcompiler <- tcompiler
      rdecompiler <- rdecompiler
      tdecompiler <- tdecompiler
      rd <- rdecompiler.decompile(tcompiler.stream(Paths.get(p), true, blocker))
      td <- tdecompiler.decompile(tcompiler.stream(Paths.get(p), true, blocker))
    } yield (rd, td)
  }.unsafeRunSync()

val (rd, td) = compdec("fibo.wat")
```

The simple raw decompiler simply prints a text version of the module sections. It can render modules that are not valid.
```scala mdoc
println(rd.render(0))
```

The smart text decompiler renders a formatted text version of the module. The module must be valid to be rendered.
```scala mdoc
println(td.render(0))
```
