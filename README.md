# Swam [![Build Status](https://travis-ci.org/satabin/swam.svg?branch=master)](https://travis-ci.org/satabin/swam) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/6cadb836067c4e4696c3c15ab9510a3a)](https://www.codacy.com/app/satabin/swam?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/swam&amp;utm_campaign=Badge_Grade)

[WebAssembly][1] implementation in Scala with [cats][7].

[![Cats Friendly Badge][6]][7]

[![Join the chat at https://gitter.im/dotnet/coreclr](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/satabin/swam?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

This project defines several modules:
 - The [`core`](core/) module is a library that makes it possible to manipulate [binary][3] representation of WebAssembly modules. In particular it contains:
   - a streaming parser for the binary format;
   - a compiler from text to binary format.
 - The [`runtime`](runtime/) module is a [non-web embedding][4] to instantiate and run WebAssembly modules.
 - The [`text`](text/) module is a library that makes it possible to manipulate [text][2] representation of WebAssembly modules.
   It is not included in the `core` module as text representation is more of a debug feature, and the runtime does not want to bring
   that dependency with it.

If you want more details, please refer to the [API documentation][5].

If you have the following `examples/logged.wat` file:
```wat
(module $logged_module
  (import "console" "log" (func $log (param i32)))
  (type (func (param $p i32) (result i32)))
  (func $add42 (type 0)
    local.get 0
    call $log
    i32.const 42
    local.get 0
    i32.add)
  (export "add42" (func $add42)))
```

and the following `examples/logged.sc` file:
```scala
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
  mod <- engine.compile(tcompiler.stream(Paths.get("examples/logged.wat"), true))
  inst <- mod.newInstance(foo)
  f <- inst.exports.typed.function1[Int, Int]("add42")
} yield f).unsafeRunSync()

println(f(4).unsafeRunSync())
```

Then you can run it this way:
```shell
$ mill -i examples.repl -c 'import $file.examples.logged'
```

All examples can be found under the [examples](examples/) directory.
For each `<name>.sc` file in the `examples` directory, you may run it with:
```shell
$ mill -i examples.repl -c 'import $file.examples.<name>'
```

[1]: https://webassembly.org/
[2]: https://webassembly.org/docs/text-format/
[3]: https://webassembly.org/docs/binary-encoding/
[4]: https://webassembly.org/docs/non-web/
[5]: https://satabin.github.io/swam/api/
[6]: https://typelevel.org/cats/img/cats-badge-tiny.png
[7]: https://typelevel.org/cats
