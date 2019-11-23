---
title: Imports
---

When a module is instantiated, it is possible to import functions or values from Scala into the module. For instance, let’s assume we defined a function that prints its parameter on the console, and a [simple WebAssembly module that imports and calls it](https://github.com/satabin/swam/blob/master/examples/docs/logged.wat).

```scala mdoc:silent
import swam._
import swam.text._
import swam.runtime._
import swam.runtime.imports._
import swam.runtime.formats.DefaultFormatters._
import cats.effect._
import java.nio.file.Paths

val tcompiler = Compiler[IO]

val engine = Engine[IO]

def log(i: Int) = IO(println(s"got $i"))

val f = (for {
  engine <- engine
  tcompiler <- tcompiler
  mod <- engine.compile(tcompiler.stream(Paths.get("logged.wat"), true))
  inst <- mod.importing("console", "log" -> log _).instantiate
  f <- inst.exports.typed.function1[Int, Int]("add42")
} yield f).unsafeRunSync()
```

running function `f` logs the parameter.
```scala mdoc
f(4).unsafeRunSync()
```

It is also possible to use [`HList`s][hlist] to represent imported modules with several exposed fields. For instance, let’s import the `console` module with an extra `colors` field exposed.

```scala mdoc:silent
import shapeless._

for {
  engine <- engine
  tcompiler <- tcompiler
  mod <- engine.compile(tcompiler.stream(Paths.get("logged.wat"), true))
  inst <- mod.importing("console", "log" -> log _ :: "colors" -> 256 :: HNil).instantiate
  f <- inst.exports.typed.function1[Int, Int]("add42")
} yield f

```

[hlist]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#heterogenous-lists
