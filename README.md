# Swam [![Build Status](https://travis-ci.org/satabin/swam.svg?branch=master)](https://travis-ci.org/satabin/swam)

[WebAssembly](https://webassembly.org/) implementation in Scala.

If you have the following `logged.wat` file:
```wat
(module $logged_module
  (import "console" "log" (func $log (param i32)))
  (type (func (param $p i32) (result i32)))
  (func $add42 (type 0)
    get_local 0
    call $log
    i32.const 42
    get_local 0
    i32.add)
  (export "add42" (func $add42)))
```

and the following `logged.sc` file:
```scala
import swam._
import swam.text._
import swam.runtime._
import swam.runtime.imports._
import swam.runtime.formats.DefaultFormatters._
import cats.effect._
import java.nio.file.Paths


val tcompiler = new Compiler[IO]

val engine = new SwamEngine[IO]

def log(i: Int) = IO(println(s"got $i"))

type AsIIO[T] = AsInterface[T, IO]

val foo = Imports[IO](module("console", TCMap[String, AsIIO]("log" -> log _)))

val f = (for {
  mod <- engine.compile(tcompiler.stream(Paths.get("logged.wat"), true))
  inst <- mod.newInstance(foo)
  f <- inst.exports.function1[Int, Int]("add42")
} yield f).unsafeRunSync()

println(f(4).unsafeRunSync())
```

Then you can run it this way:
```shell
$ mill -i runtime.repl -c 'import $file.logged'
```
