---
title: CLI
---

Swam comes with a Command Line Interface (_CLI_). In the remainder of this section, we assume you have downloaded the CLI jar file as file `swam-cli.jar`. If you don't have it yet, you can download it from the [latest release page][releases].

The CLI consists of several tools available through commands. To get the list of commands, you can run:

```shell
$ java -jar swam-cli.jar --help

Usage:
    swam-cli run
    swam-cli decompile
    swam-cli validate
    swam-cli compile

Swam from the command line

Options and flags:
    --help
        Display this help text.

Subcommands:
    run
        Run a WebAssembly file
    decompile
        Decompile a wasm file
    validate
        Validate a wasm file
    compile
        Compile a wat file to wasm
```

[releases]: https://github.com/satabin/swam/releases/latest/
