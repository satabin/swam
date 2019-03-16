(module $string
  (memory (data "A C-like string\00"
                "\05\00\00\00\73\c3\bc\c3\9f"))
  (global (export "c-like") i32 (i32.const 0))
  (global (export "utf8") i32 (i32.const 16)))
