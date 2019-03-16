# Swam [![Build Status](https://travis-ci.org/satabin/swam.svg?branch=master)](https://travis-ci.org/satabin/swam) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/6cadb836067c4e4696c3c15ab9510a3a)](https://www.codacy.com/app/satabin/swam?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/swam&amp;utm_campaign=Badge_Grade)

[WebAssembly][1] implementation in Scala with [cats][7].

[![Cats Friendly Badge][6]][7]

[![Join the chat at https://gitter.im/satabin/swam](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/satabin/swam?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

This project defines several modules:
 - The [`core`](core/) module is a library that makes it possible to manipulate [binary][3] representation of WebAssembly modules. In particular it contains:
   - a streaming parser for the binary format;
   - a compiler from text to binary format.
 - The [`runtime`](runtime/) module is a [non-web embedding][4] to instantiate and run WebAssembly modules.
 - The [`text`](text/) module is a library that makes it possible to manipulate [text][2] representation of WebAssembly modules.
   It is not included in the `core` module as text representation is more of a debug feature, and the runtime does not want to bring
   that dependency with it.

If you want more details, please refer to the [website][website].

[1]: https://webassembly.org/
[2]: https://webassembly.org/docs/text-format/
[3]: https://webassembly.org/docs/binary-encoding/
[4]: https://webassembly.org/docs/non-web/
[6]: https://typelevel.org/cats/img/cats-badge-tiny.png
[7]: https://typelevel.org/cats
[website][https://swam.gnieh.org]
