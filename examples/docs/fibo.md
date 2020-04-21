---
title: Recursion
---

Recursive functions are allowed. For instance, having defined [functions to compute fibonacci numbers](/examples/fibo.wat).

```scala mdoc:silent
import swam._
import text._
import runtime._
import formats.DefaultFormatters._
import cats.effect._
import java.nio.file.Paths

val tcompiler = Compiler[IO]

val engine = Engine[IO]()

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

def instantiate(p: String): Instance[IO] =
  Blocker[IO].use { blocker =>
    for {
      engine <- engine
      tcompiler <- tcompiler
      m <- engine.compile(tcompiler.stream(Paths.get(p), true, blocker))
      i <- m.instantiate
    } yield i
  }.unsafeRunSync()

def time[T](t: => T): T = {
  val start = System.currentTimeMillis
  val res = t
  val end = System.currentTimeMillis
  println(s"Time: ${end - start}ms")
  res
}

val i = instantiate("fibo.wat")

val naive = i.exports.typed.function1[Long, Long]("naive").unsafeRunSync()
val clever = i.exports.typed.function1[Long, Long]("clever").unsafeRunSync()
```

This would result in:

```scala mdoc
time(naive(30).unsafeRunSync())
time(clever(30).unsafeRunSync())
```
