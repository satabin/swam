(module $t
  (func $add (export "add") (param $x i32) (result i32)
   (if (result i32)
       (i32.lt_s
        (local.get $x)
        (i32.const 10)
       )
       (then
        (i32.const 10)
       )
       (else
        (local.get $x)
       )
      )
  )
)