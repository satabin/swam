(module $logged_module
  (import "m" "v1" (global $v1 i32))
  (import "m" "test" (global $test f64))
  (import "m" "v3" (global $v3 (mut i32)))
  (type (func))
  (func $mutate (type 0)
    global.get $v1
    (i32.trunc_f64_s (global.get $test))
    i32.add
    global.set $v3)
  (export "mutate" (func $mutate)))
