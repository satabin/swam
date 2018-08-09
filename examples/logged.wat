(module $logged_module
  (import "console" "log" (func $log (param i32)))
  (type (func (param $p i32) (result i32)))
  (func $add42 (type 0)
    get_local 0
    call $log
    i32.const 42
    get_local 0
    i32.add)
  (export "add42" (func $add42)))
