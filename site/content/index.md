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

If you want more details, please refer to the examples or [API documentation][api].

[webassembly]: https://webassembly.org/
[text-format]: https://webassembly.org/docs/text-format/
[binary-encoding]: https://webassembly.org/docs/binary-encoding/
[non-web-embedding]: https://webassembly.org/docs/non-web/
[api]: /api/
[cats-friendly-logo]: https://typelevel.org/cats/img/cats-badge-tiny.png
[cats]: https://typelevel.org/cats
