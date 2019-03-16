---
title: Imports
---

When a module is instantiated, it is possible to import functions or values from Scala into the module. For instance, let’s assume we defined a function that prints its parameter on the console, and a [simple WebAssembly module that imports and calls it](/examples/logged.wat).

```scala mdoc:silent
import swam._
import swam.text._
import swam.runtime._
import swam.runtime.imports._
import swam.runtime.formats.DefaultFormatters._
import cats.effect._
import java.nio.file.Paths

val tcompiler = Compiler[IO]

val engine = SwamEngine[IO]

def log(i: Int) = IO(println(s"got $i"))

type AsIIO[T] = AsInterface[T, IO]

val foo = Imports[IO](module("console", TCMap[String, AsIIO]("log" -> log _)))

val f = (for {
  engine <- engine
  tcompiler <- tcompiler
  mod <- engine.compile(tcompiler.stream(Paths.get("logged.wat"), true))
  inst <- mod.newInstance(foo)
  f <- inst.exports.typed.function1[Int, Int]("add42")
} yield f).unsafeRunSync()
```

running function `f` logs the parameter.
```scala mdoc
f(4).unsafeRunSync()
```
