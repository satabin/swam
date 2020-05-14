---
title: Recursion
---

Recursive functions are allowed. For instance, having defined [functions to compute fibonacci numbers](/examples/fibo.wat).

```scala mdoc:silent
import swam._
import text._
import runtime._
import cats.effect._
import java.nio.file.Paths

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

def instantiate(p: String): Instance[IO] =
  Blocker[IO].use { blocker =>
    for {
      engine <- Engine[IO](blocker)
      tcompiler <- Compiler[IO](blocker)
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

val naive = i.exports.typed.function[Long, Long]("naive").unsafeRunSync()
val clever = i.exports.typed.function[Long, Long]("clever").unsafeRunSync()
```

This would result in:

```scala mdoc
time(naive(30).unsafeRunSync())
time(clever(30).unsafeRunSync())
```
