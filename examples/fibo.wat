(module $fibo
  (func $naive (export "naive") (param $i i64) (result i64)
    local.get $i
    i64.const 2
    i64.lt_s
    if (result i64)
      i64.const 1
    else
      local.get $i
      i64.const 2
      i64.sub
      call $naive
      local.get $i
      i64.const 1
      i64.sub
      call $naive
      i64.add
    end)

  (func $inner (param $n2 i64) (param $n1 i64) (param $n i64) (result i64)
    (if (result i64) (i64.gt_s (local.get $n) (i64.const 0))
      (then
        local.get $n1
        local.get $n1
        local.get $n2
        i64.add
        local.get $n
        i64.const 1
        i64.sub
        call $inner)
      (else
        local.get $n2)))

  (func $clever (export "clever") (param $i i64) (result i64)
    i64.const 1
    i64.const 1
    local.get $i
    call $inner))
