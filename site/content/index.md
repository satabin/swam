---
title: Home
---

# Welcome to the Swam website

[WebAssembly][webassembly] implementation in Scala with [cats][cats].

[![Cats Friendly Badge][cats-friendly-logo]][cats]

[![Join the chat on Gitter](https://badges.gitter.im/satabin/swam.svg)](https://gitter.im/satabin/swam?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

This project defines several modules:

 - The `core` module is a library that makes it possible to manipulate [binary][binary-encoding] representation of WebAssembly modules. In particular it contains:
   - a streaming parser for the binary format;
   - a compiler from text to binary format.
 - The `runtime` module is a [non-web embedding][non-web-embedding] to instantiate and run WebAssembly modules.
 - The `text` module is a library that makes it possible to manipulate [text][text-format] representation of WebAssembly modules.
   It is not included in the `core` module as text representation is more of a debug feature, and the runtime does not want to bring
   that dependency with it.
 - The `wasi` module contains the library implementing [WASI][wasi] on top of [cats-effect][cats-effect] that can be imported by modules run within Swam.
 - The `cli` module contains a CLI tool that allows to run the tools offered by Swam from the command line.

If you want more details, please refer to the [CLI documentation][cli], the examples, or the [API documentation][api].

## Installation

Artefacts are published on maven, use your favorite build tool to bring it into your project:

 - The `core` module: [![Core module on Maven Central][core-image]][core-maven]
 - The `runtime` module: [![Runtime module on Maven Central][runtime-image]][runtime-maven]
 - The `text` module: [![Text module on Maven Central][text-image]][text-maven]

[webassembly]: https://webassembly.org/
[text-format]: https://webassembly.org/docs/text-format/
[binary-encoding]: https://webassembly.org/docs/binary-encoding/
[non-web-embedding]: https://webassembly.org/docs/non-web/
[api]: /api/
[cli]: /cli/
[cats-friendly-logo]: https://typelevel.org/cats/img/cats-badge-tiny.png
[cats]: https://typelevel.org/cats
[core-image]: https://img.shields.io/maven-central/v/org.gnieh/swam-core_2.13.svg
[core-maven]: https://maven-badges.herokuapp.com/maven-central/org.gnieh/swam-core_2.13
[runtime-image]: https://img.shields.io/maven-central/v/org.gnieh/swam-runtime_2.13.svg
[runtime-maven]: https://maven-badges.herokuapp.com/maven-central/org.gnieh/swam-runtime_2.13
[text-image]: https://img.shields.io/maven-central/v/org.gnieh/swam-text_2.13.svg
[text-maven]: https://maven-badges.herokuapp.com/maven-central/org.gnieh/swam-text_2.13
[wasi]: https://wasi.dev/
