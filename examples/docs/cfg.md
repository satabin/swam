---
title: CFG Construction and Manipulation
---

To perform analyses on WASM programs, it is possible to build a CFG of the function bodies.

## Computation

A CFG is computed using a `Traverser`, for instance to compute CFGs for the [naive fibonacci function](/examples/fibo.wat), you can do this way

```scala mdoc:silent
import swam._
import swam.text._
import swam.cfg._

import cats.effect._

import java.nio.file.Paths

val compiler = Compiler[IO]

val cfg =
  (for {
    compiler <- compiler
    naive <- compiler.compile(Paths.get("fibo.wat")).map(_.funcs(0))
    cfg <- CFGicator.buildCFG[IO](naive.body)
  } yield cfg).unsafeRunSync()
```

The CFG can be traversed in postorder (depth first) using the `CFG.postorder` function, that makes it possible to compute a value by accumulation.

For instance, to build the list of nodes in reverse postorder, one can do:

```scala mdoc
val reversePostorder = cfg.postorder(List.empty[BasicBlock])((acc, block) => block :: acc)
```

This is a common ordering when working with CFG, and is available through the `CFG.reversePostorder` method.

## Pretty Printing

For debugging or documentation purpose, it might be handy to pretty-print the CFGs.
Swam provides a [Show](https://typelevel.org/cats/typeclasses/show.html) instance that prints the CFG to a [dot](https://www.graphviz.org/) graph.

```scala mdoc
import swam.cfg.dot._

import cats.implicits._

println(cfg.show)
```

The generated code can be compiled to get an image of it. In this example, it renders as:

![Naive fibonacci CFG](/examples/fibo-naive.png)
