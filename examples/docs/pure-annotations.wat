(module
  (import "m" "add42" (func $add42 (param i32) (result i32)))
  (type (func (result i32)))
  (func $f (type 0)
    (call $add42 (i32.const 13)))
  (export "f" (func $f)))
