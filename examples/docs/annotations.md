---
title: Annotations
---

Using the `module` annotation, it is possible to mark an entire class or trait as some module that can be imported by Swam modules. The annotated type will have an implicit instance of `AsInstance` with annotated exported fields. This comes in handy to expose Scala instances to WebAssembly modules.

Following class members can be exported:
 - `val` annotated with `global` will be exported as contanst global. The `val` type must have a `ValueFormatter`.
 - `var` annotated with `global` will be exported as mutable global. The `var` type must have a `ValueFormatter`.
 - `def` annotated with `pure` or `effectful` will be exported as functions. The parameter types must have a `ValueReader` and the return type a `ValueWriter`.

By default the exported field will have the member name, but this can be changed by providing the `name` parameter.

## Exported global variables

For instance, let’s define a simple class with three exported members.

```scala mdoc:silent
import swam.runtime.imports.annotations._

@module
class MyModule {

  @global
  val v1: Int = 23

  @global(name = "test")
  val v2: Double = 2.0d

  @global
  var v3: Int = 45

}

val m = new MyModule
```

Instance `m` can be used as an [import](/examples/annotations.wat) and `v3` can be mutated from a WebAssembly module.

```scala mdoc:silent
import swam.text._
import swam.runtime._
import cats.effect._
import java.nio.file.Paths

implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

val f =
  Blocker[IO].use { blocker =>
    for {
      engine <- Engine[IO](blocker)
      tcompiler <- Compiler[IO](blocker)
      mod <- engine.compile(tcompiler.stream(Paths.get("annotations.wat"), true, blocker))
      inst <- mod.importing("m", m).instantiate
      f <- inst.exports.typed.function[Unit, Unit]("mutate")
    } yield f
  }.unsafeRunSync()
```

```scala mdoc
m.v3
f().unsafeRunSync()
m.v3
```

## Exported functions

More interestingly, functions can also be exported. They come in two flavors:
 - pure functions, annotated with `pure`;
 - and effectful functions, annotated with `effectful`.

### Pure functions

Let's assume we have a simple `add42` function, written in scala:

```scala mdoc:silent
@module
class PureModule {

  @pure
  def add42(i: Int): Int = i + 42

}

val pm = new PureModule
```

WebAssembly modules can now [import it, and call it](/examples/pure-annotations.wat):

```scala mdoc:silent
val add42 =
  Blocker[IO].use { blocker =>
    for {
      engine <- Engine[IO](blocker)
      tcompiler <- Compiler[IO](blocker)
      mod <- engine.compile(tcompiler.stream(Paths.get("pure-annotations.wat"), true, blocker))
      inst <- mod.importing("m", pm).instantiate
      f <- inst.exports.typed.function[Unit, Int]("f")
    } yield f
  }.unsafeRunSync()
```

Executing the imported function, returns the desired result:
```scala mdoc
add42().unsafeRunSync()
```

### Effectful functions

Functions that may raise exceptions or perform side-effects, should return their result wrapped in an effect type `F[_]`. Modules with such functions must have the effect type as type parameter, annotated with `effect`.
This type must be unique, and all effectful functions must use it.

We can define a logging function in scala as follows, that simply prints to stdout:

```scala mdoc:silent
import cats._

@module
class EffectfulModule[@effect F[_]](implicit F: Applicative[F]) {

  @effectful
  def log(i: Int): F[Unit] = F.pure(println(s"got: $i"))

}

val mIO = new EffectfulModule[IO]
```

Now, WebAssembly modules can [import that module and use the effectful function](/examples/effectful-annotations.wat):

```scala mdoc:silent
val logged =
  Blocker[IO].use { blocker =>
    for {
      engine <- Engine[IO](blocker)
      tcompiler <- Compiler[IO](blocker)
      mod <- engine.compile(tcompiler.stream(Paths.get("effectful-annotations.wat"), true, blocker))
      inst <- mod.importing("console", mIO).instantiate
      f <- inst.exports.typed.function[Int, Int]("add42")
    } yield f
  }.unsafeRunSync()
```

We can now run the `logged` function and the parameter is logged to stdout as expected
```scala mdoc
logged(43).unsafeRunSync()
```
