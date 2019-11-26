# Swam [![Build Status](https://travis-ci.org/satabin/swam.svg?branch=master)](https://travis-ci.org/satabin/swam) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/6cadb836067c4e4696c3c15ab9510a3a)](https://www.codacy.com/app/satabin/swam?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/swam&amp;utm_campaign=Badge_Grade)

[WebAssembly][1] implementation in Scala with [cats][7].

[![Cats Friendly Badge][6]][7]

[![Join the chat at https://gitter.im/satabin/swam](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/satabin/swam?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

## Getting Started

To build it, we use [mill](http://www.lihaoyi.com/mill/). Mill is not packaged for all Linux distributions, but you can use curl to install it (see Installation instructions in the [doc](http://www.lihaoyi.com/mill/)).

You cann also use the [mill wrapper](https://github.com/lefou/millw) provided in this repository with the recommended mill version. It will download mill for you if you do not have it already, and run it. To use it, replace following `mill` commands by `./millw`

If you want to run the specification tests, just type:

```sh
mill runtime.test.low
```

It you want to test swam in a REPL session, just type tostart an ammonite shell with runtime project in classpath.:

```sh
mill -i runtime.repl
```

If you want a REPL session with both runtime and text, the easiest way is to start a session for the runtime.test project:

    $ mill -i runtime.test.repl

## Architecture

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
[website]: https://swam.gnieh.org
