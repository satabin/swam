package swam
package runtime
package internals
package instance

import scala.collection.mutable.ListBuffer

object WasiFilter {

  /**
   * Functions checks whether the function name contains the patterns or not.
   * @return true or false based on 
   */  
  def checkPattern(pattern: String, functionName: String) : Boolean = {
    val pat = pattern.r
    val checkSome = pat findFirstIn functionName
    if(checkSome.isEmpty){
      true
    }
    else{
      false
    }
  }

  /**
   * Reads the defined and undefined files in wasi and returns the Set of function defined in these files.
   * @return
   */
  def readWasiFile() : Set[String] = {
    val defined_check = scala.reflect.io.File("runtime/src/swam/runtime/internals/instance/defined-symbols.txt").exists
    val undefined_check = scala.reflect.io.File("runtime/src/swam/runtime/internals/instance/undefined-symbols.txt").exists
    var define : Set[String] = Set() 
    if(defined_check && undefined_check){ 
      val defined= scala.io.Source.fromFile("runtime/src/swam/runtime/internals/instance/defined-symbols.txt", "UTF-8").mkString
      val undefined= scala.io.Source.fromFile("runtime/src/swam/runtime/internals/instance/undefined-symbols.txt", "UTF-8").mkString  
      val undefine = undefined.split("\n").toSet
      define = defined.split("\n").toSet
      
      define = define ++ undefine
      /**
      * Set of wasi methods and musl methods that are injected in wasm module.
      * __orginal_main is the main function
      *
      */
      define ++= List("dummy",
                    "long_double_not_supported",
                    "dlmalloc",
                    "dispose_chunk",
                    "internal_memalign",
                    "dlfree",
      /*----------------------------------------musl compiler-rt ABI's--------------------------------*/
      /*https://opensource.apple.com/source/clang/clang-800.0.38/src/projects/compiler-rt/lib/builtins*/
                    "__absvdi2","__absvsi2","__absvti2","__adddf3",
                    " __addsf3","__addtf3","__addvdi3","__addvsi3",
                    "__addvti3","__ashldi3","__ashlti3","__ashrdi3",
                    "__ashrti3","atomic_flag_clear","atomic_flag_clear_explicit",
                    "atomic_flag_test_and_set","atomic_flag_test_and_set_explicit",
                    "atomic_signal_fence","atomic_thread_fence","__bswapdi2","__bswapsi2",
                    "__clzdi2","__clzsi2","__clzti2","__cmpdi2","__cmpti2","__ctzdi2",
                    "__ctzsi2"," __ctzti2","__divdc3","__divdf3","__divdi3",
                    "__divmoddi4","__divmodsi4","__divsc3","__divsf3","__divsi3",
                    "__divtc3","__divtf3","__divti3","__divxc3","__enable_execute_stack",
                    "__eprintf","__extenddftf2","__extendhfsf2","__gnu_h2f_ieee","__extendsfdf2",
                    "__extendsftf2","__ffsdi2","__ffssi2","__ffsti2"," __fixunsdfdi","__fixdfdi",
                    "__fixdfsi","__fixdfti","__fixunssfdi","__fixsfdi","__fixsfsi",
                    "__fixsfti","_fixtfdi","__fixtfsi","__fixtfti","__fixunsdfdi","__fixunsdfdi",
                    "__aeabi_d2ulz","__fixunsdfsi","__fixunsdfti","__fixunssfdi","__fixunssfsi",
                    "__fixunssfti","__fixunstfdi","__fixunstfsi","__fixunstfti","__fixunsxfdi",
                    "__fixunsxfsi","__fixunsxfti","__fixxfdi","__fixxfti","__floatdidf",
                    "__floatdidf",
                    "__aeabi_l2d","__floatdisf","__aeabi_l2f","__floatditf","__floatdixf",
                    "__floatsidf","__floatsisf","__floatsitf","__floattidf","__floattisf",
                    "__floattitf"," __floatundidf","__floatundisf","__floatunditf",
                    "__floatundixf","__floatunsidf","__floatunsisf","__floatunsitf",
                    "__floatuntidf","__floatuntisf","__floatuntitf","__floatuntixf","__addXf3__",
                    "__extendXfYf2__","__fixint","__fixuint","__fe_getround","__mulXf3__",
                    "__truncXfYf2__","__udivXi3","__umodXi3","__paritysi2","__paritydi2",
                    "__compilerrt_abort_impl"," __lshrdi3","__lshrti3","__moddi3","__modti3",
                    "__modsi3","__muldf3","__muldsi3","__mulodi4","__mulosi4","__muloti4",
                    "__mulsc3","__mulsf3",
                    "__multc3","__multf3","__mulddi3","__mulvdi3","__mulvsi3","__mulvti3",
                    "__mulxc3","__negdf2","__negdi2","__negsf2","__negti2","__negvdi2",
                    "__negvsi2","__negvti2","__popcountdi2","__popcountsi2","__popcountti2",
                    "__powidf2","__powisf2","__powitf2","__powixf2","__subdf3",
                    "__subsf3","__subtf3","__subvdi3","__subvsi3","__subvti3","__truncdfhf2",
                    "__truncdfsf2","__truncsfhf2","__trunctfdf2","__trunctfsf2","__ucmpdi2",
                    "__ucmpti2","__udivdi3","__udivmoddi4","__udivmodsi4","__udivmodti4",
                    "__udivsi3","__umoddi3","__umodsi3","__umodti3",
                    "__muldc3",
      /*-------------musl standard library for Linux-based systems-------------*/
      /*https://git.musl-libc.org/cgit/musl/tree/src*/
                 "sn_write","pop_arg","printf_core","scanexp","do_read")
      define -= "__original_main"
    }
    define
  }
}
