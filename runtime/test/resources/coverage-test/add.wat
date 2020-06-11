(module $addt
  (func $add (export "add") (param $i i32) (param $j i32) (result i32)
    local.get $i
    local.get $j
    i32.add 
  )
)