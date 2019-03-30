---
title: Annotations
---

Using the `module` annotation, it is possible to mark an entire class or trait as some module that can be imported by Swam modules. The annotated type will have an implicit instance of `AsInstance` with annotated exported fields. This comes in handy to expose Scala instances to WebAssembly modules.

Following class members can be exported:
 - `val` annotated with `global` will be exported as contanst global. The `val` type must have a `SimpleValueFormatter`.
 - `var` annotated with `global` will be exported as mutable global. The `var` type must have a `SimpleValueFormatter`.

By default the exported field will have the member name, but this can be changed by providing the `name` parameter.

For instance, let’s define a simple class with three exported members.

```scala mdoc:silent
import swam.runtime.imports.annotations._
import swam.runtime.formats.DefaultFormatters._

@module
class MyModule {

  @swam.runtime.imports.annotations.global
  val v1 = 23

  @swam.runtime.imports.annotations.global(name = "test")
  val v2 = 2.0d

  @swam.runtime.imports.annotations.global
  var v3 = 45

}

val m = new MyModule
```

Instance `m` can be used as an [import](/examples/annotations.wat) and `v3` can be mutated from a WebAssembly module.

```scala mdoc:silent
import swam._
import swam.text._
import swam.runtime._
import swam.runtime.imports._
import cats.effect._
import java.nio.file.Paths

val tcompiler = Compiler[IO]

val engine = Engine[IO]

val f = (for {
  engine <- engine
  tcompiler <- tcompiler
  mod <- engine.compile(tcompiler.stream(Paths.get("annotations.wat"), true))
  inst <- mod.importing("m", m).instantiate
  f <- inst.exports.typed.procedure0("mutate")
} yield f).unsafeRunSync()
```

```scala mdoc
m.v3
f().unsafeRunSync()
m.v3
```
