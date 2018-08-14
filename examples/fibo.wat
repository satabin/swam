(module $fibo
  (func $naive (export "naive") (param $i i64) (result i64)
    get_local $i
    i64.const 2
    i64.lt_s
    if (result i64)
      i64.const 1
    else
      get_local $i
      i64.const 2
      i64.sub
      call $naive
      get_local $i
      i64.const 1
      i64.sub
      call $naive
      i64.add
    end)
  
  (func $inner (param $n2 i64) (param $n1 i64) (param $n i64) (result i64)
    (if (result i64) (i64.gt_s (get_local $n) (i64.const 0))
      (then
        get_local $n1
        get_local $n1
        get_local $n2
        i64.add
        get_local $n
        i64.const 1
        i64.sub
        call $inner)
      (else
        get_local $n2)))

  (func $clever (export "clever") (param $i i64) (result i64)
    i64.const 1
    i64.const 1
    get_local $i
    call $inner))
