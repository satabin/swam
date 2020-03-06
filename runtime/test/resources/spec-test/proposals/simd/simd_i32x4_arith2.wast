;; Tests for i32x4 [min_s, min_u, max_s, max_u] operations.

(module
  (func (export "i32x4.min_s") (param v128 v128) (result v128) (i32x4.min_s (local.get 0) (local.get 1)))
  (func (export "i32x4.min_u") (param v128 v128) (result v128) (i32x4.min_u (local.get 0) (local.get 1)))
  (func (export "i32x4.max_s") (param v128 v128) (result v128) (i32x4.max_s (local.get 0) (local.get 1)))
  (func (export "i32x4.max_u") (param v128 v128) (result v128) (i32x4.max_u (local.get 0) (local.get 1)))
  (func (export "i32x4.min_s_with_const_0") (result v128) (i32x4.min_s (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295) (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648)))
  (func (export "i32x4.min_s_with_const_1") (result v128) (i32x4.min_s (v128.const i32x4 0 1 2 3) (v128.const i32x4 3 2 1 0)))
  (func (export "i32x4.min_u_with_const_2") (result v128) (i32x4.min_u (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295) (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648)))
  (func (export "i32x4.min_u_with_const_3") (result v128) (i32x4.min_u (v128.const i32x4 0 1 2 3) (v128.const i32x4 3 2 1 0)))
  (func (export "i32x4.max_s_with_const_4") (result v128) (i32x4.max_s (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295) (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648)))
  (func (export "i32x4.max_s_with_const_5") (result v128) (i32x4.max_s (v128.const i32x4 0 1 2 3) (v128.const i32x4 3 2 1 0)))
  (func (export "i32x4.max_u_with_const_6") (result v128) (i32x4.max_u (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295) (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648)))
  (func (export "i32x4.max_u_with_const_7") (result v128) (i32x4.max_u (v128.const i32x4 0 1 2 3) (v128.const i32x4 3 2 1 0)))
  (func (export "i32x4.min_s_with_const_8") (param v128) (result v128) (i32x4.min_s (local.get 0) (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)))
  (func (export "i32x4.min_s_with_const_9") (param v128) (result v128) (i32x4.min_s (local.get 0) (v128.const i32x4 0 1 2 3)))
  (func (export "i32x4.min_u_with_const_10") (param v128) (result v128) (i32x4.min_u (local.get 0) (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)))
  (func (export "i32x4.min_u_with_const_11") (param v128) (result v128) (i32x4.min_u (local.get 0) (v128.const i32x4 0 1 2 3)))
  (func (export "i32x4.max_s_with_const_12") (param v128) (result v128) (i32x4.max_s (local.get 0) (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)))
  (func (export "i32x4.max_s_with_const_13") (param v128) (result v128) (i32x4.max_s (local.get 0) (v128.const i32x4 0 1 2 3)))
  (func (export "i32x4.max_u_with_const_14") (param v128) (result v128) (i32x4.max_u (local.get 0) (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)))
  (func (export "i32x4.max_u_with_const_15") (param v128) (result v128) (i32x4.max_u (local.get 0) (v128.const i32x4 0 1 2 3)))
)

(assert_return (invoke "i32x4.min_s" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0 0 0 0))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 -1 -1 -1 -1))
                                     (v128.const i32x4 -1 -1 -1 -1))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 0 0 -1 -1)
                                     (v128.const i32x4 0 -1 0 -1))
                                     (v128.const i32x4 0 -1 -1 -1))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0xffffffff 0xffffffff 0xffffffff 0xffffffff))
                                     (v128.const i32x4 0xffffffff 0xffffffff 0xffffffff 0xffffffff))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 1 1 1 1)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 4294967295 4294967295 4294967295 4294967295))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 128 128 128 128))
                                     (v128.const i32x4 4294967295 4294967295 4294967295 4294967295))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 2147483648 2147483648 2147483648 2147483648)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 2147483648 2147483648 2147483648 2147483648))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 123 123 123 123)
                                     (v128.const i32x4 01_2_3 01_2_3 01_2_3 01_2_3))
                                     (v128.const i32x4 123 123 123 123))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 0x80 0x80 0x80 0x80)
                                     (v128.const i32x4 0x0_8_0 0x0_8_0 0x0_8_0 0x0_8_0))
                                     (v128.const i32x4 0x80 0x80 0x80 0x80))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0 0 0 0))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 -1 -1 -1 -1))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 0 0 -1 -1)
                                     (v128.const i32x4 0 -1 0 -1))
                                     (v128.const i32x4 0 0 0 -1))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0xffffffff 0xffffffff 0xffffffff 0xffffffff))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 1 1 1 1)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 128 128 128 128))
                                     (v128.const i32x4 128 128 128 128))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 2147483648 2147483648 2147483648 2147483648)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 2147483648 2147483648 2147483648 2147483648))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 123 123 123 123)
                                     (v128.const i32x4 01_2_3 01_2_3 01_2_3 01_2_3))
                                     (v128.const i32x4 123 123 123 123))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 0x80 0x80 0x80 0x80)
                                     (v128.const i32x4 0x0_8_0 0x0_8_0 0x0_8_0 0x0_8_0))
                                     (v128.const i32x4 0x80 0x80 0x80 0x80))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0 0 0 0))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 -1 -1 -1 -1))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 0 0 -1 -1)
                                     (v128.const i32x4 0 -1 0 -1))
                                     (v128.const i32x4 0 0 0 -1))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0xffffffff 0xffffffff 0xffffffff 0xffffffff))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 1 1 1 1)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 128 128 128 128))
                                     (v128.const i32x4 128 128 128 128))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 2147483648 2147483648 2147483648 2147483648)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 2147483648 2147483648 2147483648 2147483648))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 123 123 123 123)
                                     (v128.const i32x4 01_2_3 01_2_3 01_2_3 01_2_3))
                                     (v128.const i32x4 123 123 123 123))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 0x80 0x80 0x80 0x80)
                                     (v128.const i32x4 0x0_8_0 0x0_8_0 0x0_8_0 0x0_8_0))
                                     (v128.const i32x4 0x80 0x80 0x80 0x80))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0 0 0 0))
                                     (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 -1 -1 -1 -1))
                                     (v128.const i32x4 -1 -1 -1 -1))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 0 0 -1 -1)
                                     (v128.const i32x4 0 -1 0 -1))
                                     (v128.const i32x4 0 -1 -1 -1))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 0 0 0 0)
                                     (v128.const i32x4 0xffffffff 0xffffffff 0xffffffff 0xffffffff))
                                     (v128.const i32x4 0xffffffff 0xffffffff 0xffffffff 0xffffffff))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 1 1 1 1)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 1 1 1 1))
                                     (v128.const i32x4 4294967295 4294967295 4294967295 4294967295))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 4294967295 4294967295 4294967295 4294967295)
                                     (v128.const i32x4 128 128 128 128))
                                     (v128.const i32x4 4294967295 4294967295 4294967295 4294967295))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 2147483648 2147483648 2147483648 2147483648)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 2147483648 2147483648 2147483648 2147483648))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000)
                                     (v128.const i32x4 -2147483648 -2147483648 -2147483648 -2147483648))
                                     (v128.const i32x4 0x80000000 0x80000000 0x80000000 0x80000000))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 123 123 123 123)
                                     (v128.const i32x4 01_2_3 01_2_3 01_2_3 01_2_3))
                                     (v128.const i32x4 123 123 123 123))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 0x80 0x80 0x80 0x80)
                                     (v128.const i32x4 0x0_8_0 0x0_8_0 0x0_8_0 0x0_8_0))
                                     (v128.const i32x4 0x80 0x80 0x80 0x80))

;; Const vs const
(assert_return (invoke "i32x4.min_s_with_const_0") (v128.const i32x4 -2147483648 1073741824 1073741824 -2147483648))
(assert_return (invoke "i32x4.min_s_with_const_1") (v128.const i32x4 0 1 1 0))
(assert_return (invoke "i32x4.min_u_with_const_2") (v128.const i32x4 -2147483648 1073741824 1073741824 -2147483648))
(assert_return (invoke "i32x4.min_u_with_const_3") (v128.const i32x4 0 1 1 0))
(assert_return (invoke "i32x4.max_s_with_const_4") (v128.const i32x4 4294967295 2147483647 2147483647 4294967295))
(assert_return (invoke "i32x4.max_s_with_const_5") (v128.const i32x4 3 2 2 3))
(assert_return (invoke "i32x4.max_u_with_const_6") (v128.const i32x4 4294967295 2147483647 2147483647 4294967295))
(assert_return (invoke "i32x4.max_u_with_const_7") (v128.const i32x4 3 2 2 3))

;; Param vs const
(assert_return (invoke "i32x4.min_s_with_const_8" (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                                  (v128.const i32x4 -2147483648 1073741824 1073741824 -2147483648))
(assert_return (invoke "i32x4.min_s_with_const_9" (v128.const i32x4 3 2 1 0))
                                                  (v128.const i32x4 0 1 1 0))
(assert_return (invoke "i32x4.min_u_with_const_10" (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                                   (v128.const i32x4 -2147483648 1073741824 1073741824 -2147483648))
(assert_return (invoke "i32x4.min_u_with_const_11" (v128.const i32x4 3 2 1 0))
                                                   (v128.const i32x4 0 1 1 0))
(assert_return (invoke "i32x4.max_s_with_const_12" (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                                   (v128.const i32x4 4294967295 2147483647 2147483647 4294967295))
(assert_return (invoke "i32x4.max_s_with_const_13" (v128.const i32x4 3 2 1 0))
                                                   (v128.const i32x4 3 2 2 3))
(assert_return (invoke "i32x4.max_u_with_const_14" (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                                   (v128.const i32x4 4294967295 2147483647 2147483647 4294967295))
(assert_return (invoke "i32x4.max_u_with_const_15" (v128.const i32x4 3 2 1 0))
                                                   (v128.const i32x4 3 2 2 3))

;; Test different lanes go through different if-then clauses
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)
                                     (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                     (v128.const i32x4 -2147483648 1073741824 1073741824 -2147483648))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 0 1 2 128)
                                     (v128.const i32x4 0 2 1 128))
                                     (v128.const i32x4 0 1 1 128))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)
                                     (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                     (v128.const i32x4 -2147483648 1073741824 1073741824 -2147483648))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 0 1 2 128)
                                     (v128.const i32x4 0 2 1 128))
                                     (v128.const i32x4 0 1 1 128))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)
                                     (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                     (v128.const i32x4 4294967295 2147483647 2147483647 4294967295))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 0 1 2 128)
                                     (v128.const i32x4 0 2 1 128))
                                     (v128.const i32x4 0 2 2 128))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 -2147483648 2147483647 1073741824 4294967295)
                                     (v128.const i32x4 4294967295 1073741824 2147483647 -2147483648))
                                     (v128.const i32x4 4294967295 2147483647 2147483647 4294967295))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 0 1 2 128)
                                     (v128.const i32x4 0 2 1 128))
                                     (v128.const i32x4 0 2 2 128))

;; Test opposite signs of zero
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 -0 -0 +0 +0)
                                     (v128.const i32x4 +0 0 -0 0))
                                     (v128.const i32x4 -0 -0 +0 +0))
(assert_return (invoke "i32x4.min_s" (v128.const i32x4 -0 -0 -0 -0)
                                     (v128.const i32x4 +0 +0 +0 +0))
                                     (v128.const i32x4 -0 -0 -0 -0))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 -0 -0 +0 +0)
                                     (v128.const i32x4 +0 0 -0 0))
                                     (v128.const i32x4 -0 -0 +0 +0))
(assert_return (invoke "i32x4.min_u" (v128.const i32x4 -0 -0 -0 -0)
                                     (v128.const i32x4 +0 +0 +0 +0))
                                     (v128.const i32x4 -0 -0 -0 -0))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 -0 -0 +0 +0)
                                     (v128.const i32x4 +0 0 -0 0))
                                     (v128.const i32x4 -0 -0 +0 +0))
(assert_return (invoke "i32x4.max_s" (v128.const i32x4 -0 -0 -0 -0)
                                     (v128.const i32x4 +0 +0 +0 +0))
                                     (v128.const i32x4 -0 -0 -0 -0))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 -0 -0 +0 +0)
                                     (v128.const i32x4 +0 0 -0 0))
                                     (v128.const i32x4 -0 -0 +0 +0))
(assert_return (invoke "i32x4.max_u" (v128.const i32x4 -0 -0 -0 -0)
                                     (v128.const i32x4 +0 +0 +0 +0))
                                     (v128.const i32x4 -0 -0 -0 -0))

;; Unknown operators
(assert_malformed (module quote "(memory 1) (func (result v128) (f32x4.min_s (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (f32x4.min_u (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (f32x4.max_s (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (f32x4.max_u (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (i64x2.min_s (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (i64x2.min_u (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (i64x2.max_s (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (i64x2.max_u (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (f64x2.min_s (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (f64x2.min_u (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (f64x2.max_s (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")
(assert_malformed (module quote "(memory 1) (func (result v128) (f64x2.max_u (v128.const i32x4 0 0 0 0) (v128.const i32x4 1 1 1 1)))") "unknown operator")

;; Type check
(assert_invalid (module (func (result v128) (i32x4.min_s (i32.const 0) (f32.const 0.0)))) "type mismatch")
(assert_invalid (module (func (result v128) (i32x4.min_u (i32.const 0) (f32.const 0.0)))) "type mismatch")
(assert_invalid (module (func (result v128) (i32x4.max_s (i32.const 0) (f32.const 0.0)))) "type mismatch")
(assert_invalid (module (func (result v128) (i32x4.max_u (i32.const 0) (f32.const 0.0)))) "type mismatch")

;; Test operation with empty argument

(assert_invalid
  (module
    (func $i32x4.min_s-1st-arg-empty (result v128)
      (i32x4.min_s (v128.const i32x4 0 0 0 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $i32x4.min_s-arg-empty (result v128)
      (i32x4.min_s)
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $i32x4.min_u-1st-arg-empty (result v128)
      (i32x4.min_u (v128.const i32x4 0 0 0 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $i32x4.min_u-arg-empty (result v128)
      (i32x4.min_u)
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $i32x4.max_s-1st-arg-empty (result v128)
      (i32x4.max_s (v128.const i32x4 0 0 0 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $i32x4.max_s-arg-empty (result v128)
      (i32x4.max_s)
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $i32x4.max_u-1st-arg-empty (result v128)
      (i32x4.max_u (v128.const i32x4 0 0 0 0))
    )
  )
  "type mismatch"
)
(assert_invalid
  (module
    (func $i32x4.max_u-arg-empty (result v128)
      (i32x4.max_u)
    )
  )
  "type mismatch"
)

;; Combination
(module
  (func (export "i32x4.min_s-i32x4.max_u") (param v128 v128 v128) (result v128) (i32x4.min_s (i32x4.max_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.min_s-i32x4.max_s") (param v128 v128 v128) (result v128) (i32x4.min_s (i32x4.max_s (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.min_s-i32x4.min_u") (param v128 v128 v128) (result v128) (i32x4.min_s (i32x4.min_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.min_s-i32x4.min_s") (param v128 v128 v128) (result v128) (i32x4.min_s (i32x4.min_s (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.min_u-i32x4.max_u") (param v128 v128 v128) (result v128) (i32x4.min_u (i32x4.max_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.min_u-i32x4.max_s") (param v128 v128 v128) (result v128) (i32x4.min_u (i32x4.max_s (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.min_u-i32x4.min_u") (param v128 v128 v128) (result v128) (i32x4.min_u (i32x4.min_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.min_u-i32x4.min_s") (param v128 v128 v128) (result v128) (i32x4.min_u (i32x4.min_s (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_s-i32x4.max_u") (param v128 v128 v128) (result v128) (i32x4.max_s (i32x4.max_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_s-i32x4.max_s") (param v128 v128 v128) (result v128) (i32x4.max_s (i32x4.max_s (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_s-i32x4.min_u") (param v128 v128 v128) (result v128) (i32x4.max_s (i32x4.min_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_s-i32x4.min_s") (param v128 v128 v128) (result v128) (i32x4.max_s (i32x4.min_s (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_u-i32x4.max_u") (param v128 v128 v128) (result v128) (i32x4.max_u (i32x4.max_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_u-i32x4.max_s") (param v128 v128 v128) (result v128) (i32x4.max_u (i32x4.max_s (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_u-i32x4.min_u") (param v128 v128 v128) (result v128) (i32x4.max_u (i32x4.min_u (local.get 0) (local.get 1))(local.get 2)))
  (func (export "i32x4.max_u-i32x4.min_s") (param v128 v128 v128) (result v128) (i32x4.max_u (i32x4.min_s (local.get 0) (local.get 1))(local.get 2)))
)

(assert_return (invoke "i32x4.min_s-i32x4.max_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.min_s-i32x4.max_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.min_s-i32x4.min_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.min_s-i32x4.min_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.min_u-i32x4.max_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.min_u-i32x4.max_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 1 1 1 1))
(assert_return (invoke "i32x4.min_u-i32x4.min_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.min_u-i32x4.min_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 0 0 0 0))
(assert_return (invoke "i32x4.max_s-i32x4.max_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
(assert_return (invoke "i32x4.max_s-i32x4.max_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
(assert_return (invoke "i32x4.max_s-i32x4.min_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
(assert_return (invoke "i32x4.max_s-i32x4.min_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
(assert_return (invoke "i32x4.max_u-i32x4.max_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
(assert_return (invoke "i32x4.max_u-i32x4.max_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
(assert_return (invoke "i32x4.max_u-i32x4.min_u" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
(assert_return (invoke "i32x4.max_u-i32x4.min_s" (v128.const i32x4 0 0 0 0)
                                                 (v128.const i32x4 1 1 1 1)
                                                 (v128.const i32x4 2 2 2 2))
                                                 (v128.const i32x4 2 2 2 2))
