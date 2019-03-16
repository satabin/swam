(module $logged_module
  (import "console" "log" (func $log (param i32)))
  (type (func (param $p i32) (result i32)))
  (func $add42 (type 0)
    local.get 0
    call $log
    i32.const 42
    local.get 0
    i32.add)
  (export "add42" (func $add42)))
