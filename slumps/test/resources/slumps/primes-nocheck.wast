(module
  (type $t0 (func (param i32) (result i32)))
  (type $t1 (func (param i32 i32) (result i32)))
  (type $t2 (func (param i32)))
  (import "env" "memory" (memory $env.memory 4096))
  (func $stackRestore (type $t2) (param $p0 i32)
    i32.const 1024
    get_local $p0
    i32.store)
  (func $stackAlloc (type $t0) (param $p0 i32) (result i32)
    (local $l0 i32)
    i32.const 1024
    i32.const 1024
    i32.load
    get_local $p0
    i32.sub
    i32.const -16
    i32.and
    tee_local $l0
    i32.store
    get_local $l0)
  (func $main (type $t1) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l0 i32) (local $l1 i32) (local $l2 f32) (local $l3 f32)
    block $B0
      block $B1
        block $B2
          block $B3
            block $B4
              block $B5
                get_local $p0
                i32.const 2
                i32.lt_s
                br_if $B5
                get_local $p1
                i32.load offset=4
                i32.load8_s
                i32.const -48
                i32.add
                tee_local $p1
                i32.const 5
                i32.gt_u
                br_if $B4
                i32.const 33000
                set_local $l0
                i32.const 0
                set_local $p0
                block $B6
                  get_local $p1
                  i32.const 1
                  i32.sub
                  br_table $B1 $B6 $B5 $B3 $B2 $B0
                end
                i32.const 130000
                set_local $l0
                br $B1
              end
              i32.const 220000
              set_local $l0
              br $B1
            end
            i32.const -1
            return
          end
          i32.const 610000
          set_local $l0
          br $B1
        end
        i32.const 1010000
        set_local $l0
      end
      i32.const 2
      set_local $p1
      loop $L7
        block $B8
          get_local $p1
          f32.convert_s/i32
          f32.sqrt
          tee_local $l2
          f32.const 0x1p+1 (;=2;)
          f32.gt
          i32.const 1
          i32.xor
          i32.eqz
          if $I9
            i32.const 3
            set_local $p0
            loop $L10
              get_local $p1
              get_local $p0
              i32.const -1
              i32.add
              i32.rem_u
              i32.eqz
              br_if $B8
              get_local $p0
              f32.convert_s/i32
              set_local $l3
              get_local $p0
              i32.const 1
              i32.add
              set_local $p0
              get_local $l2
              get_local $l3
              f32.gt
              i32.const 1
              i32.xor
              i32.eqz
              br_if $L10
            end
          end
          get_local $l1
          i32.const 1
          i32.add
          set_local $l1
        end
        get_local $p1
        i32.const 1
        i32.add
        set_local $p1
        get_local $l1
        get_local $l0
        i32.lt_s
        br_if $L7
      end
      i32.const 0
      set_local $p0
    end
    i32.const 0)
  (export "main" (func $main))
  (export "stackAlloc" (func $stackAlloc))
  (export "stackRestore" (func $stackRestore)))
