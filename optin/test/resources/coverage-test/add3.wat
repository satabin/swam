(module $t
  (func $add (export "add") (param $x i32) (param $y i32) (result i32)
   (if (result i32)
       (i32.lt_s
        (local.get $x)
        (i32.const 10)
       )
       (then
        (i32.add
        (i32.const 10)
        (local.get $x)
        )
       )
       (else
        (if (result i32)
               (i32.lt_s
                (local.get $y)
                (i32.const 20)
               )
               (then
                (i32.add
                (i32.const 20)
                (local.get $x)
                )
               )
               (else
                (local.get $y)
               )
              )
       )
      )
  )
)