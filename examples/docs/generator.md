---
title: Boilerplates Generator
---

It is possible in swam to generate code boilerplates to implement imports for WebAssembly binaries execution. The module `generator` provides a cli tool to do so. To check available options in the `generator` cli, run `mill generator.run --help`.


## Generating scala code boilerplate for WASM binaries

For instance, let’s have a WASM binary called `posix.wasm`. Running `run generator.run posix.wasm` will generate the following output. 

```
import cats.effect.IO
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._

trait GeneratedImports {
  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  def wasi_unstableFd_write(p0: Int, p1: Int, p2: Int, p3: Int): IO[Int]
  def wasi_unstableFd_close(p0: Int): IO[Int]
  def wasi_unstableFd_fdstat_get(p0: Int, p1: Int): IO[Int]
  def wasi_unstableFd_seek(p0: Int, p1: Long, p2: Int, p3: Int): IO[Int]
  def imports() = {
    Imports[IO](
      TCMap[String, AsIsIO](
        "wasi_unstable" -> TCMap[String, AsIIO]("fd_write" -> wasi_unstableFd_write _,
                                                "fd_close" -> wasi_unstableFd_close _,
                                                "fd_fdstat_get" -> wasi_unstableFd_fdstat_get _,
                                                "fd_seek" -> wasi_unstableFd_seek _))
    )
  }
}

```

By default, the generator cli will find imported functions, then it generates a Scala language boilerplate. The generated template represents a Scala trait by default as you can see in the snippet.

The cli supports several WASM binaries as arguments, generating then, the composition of all function imports. 


## Replacing the trait template

We generate the funcion imports based trait using the [mustache template engine](https://mustache.github.io/). Mustache context is usually a dictionary. We provide the context in the following format.

```ts
{
  "module" : str,
  "comma" : boolean,
  "fields" : 
    {
        "name":  str,
        "return": str,
        "params": str,
        "nameCapital": str,
        "comma": boolean
    }[]
}[]

```

To print the context provided in json format, run the command `mill generator.run -c true <wasm file>`. The used mustache template can be replaced, using the `--template <value>` option.

## Parsing WITX



> The [WITX](https://github.com/WebAssembly/WASI/blob/master/docs/witx.md) 
 file format is an experimental format which is based on the module types text format (wit), (which is in turn based on the wat format, which is based on S-expressions). It adds some features using the same syntax as interface types, some features with syntax similar to gc types, as well as a few special features of its own. witx is actively evolving. Expect backwards-incompatible changes, particularly in the areas where witx differs from wit. The initial goal for witx is just to have a language suitable for expressing WASI APIs in, to serve as the vocabulary for proposing changes to existing APIs and proposing new APIs. Initially, while it uses some of the syntax and concepts from interface types, it doesn't currently imply the full interface types specification, or the use of the interface types custom sections. We expect that eventually we will transition to using the full interface types specification. Until then, the goals here are to remain aligned with interface types and other relevant WebAssembly standards and proposals wherever practical, and to be an input into the design process of interface types.

The generator cli also provides the way to parse witx files and generate the boilerplate project. To generate the boilerplate, run `mill generator.run -x true  -p <create_project_at> <witx_file>`. This command will generate two scala files, `Types.cala` and `Module.scala`, containing th types definitions and abstract function declarations, respectively. 

The `text` module provides the core implementation to parse this kind of files, specifically, the `swam.witx.WitxParser` class. This core component provides a small AST structure from the parsed witx file. The components of this AST can be seen in `swam.witx.unresolved.Declarations` class. Therefore, this AST can be translated to any target ,for instance, the generator cli implements the AST traversers to generate the scala files for the boilerplate. 
