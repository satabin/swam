package swam
package runtime
package internals
package instance

//import swam.optin.coverage.{ModuleCoverageInfo,ModuleShowMap}

import scala.collection.mutable.ListBuffer

object WasiFilter {

  //implicit val contextShift: ContextShift[IO] = IO.contextShift(global)
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
      /**remove non wasi methods from the and keep
      * __orginal_main is the main function
      *
      */
      define ++= List("dummy",
                    "long_double_not_supported",
                    "dlmalloc",
                    "pop_arg",
                    "printf_core",
                    "dispose_chunk",
                    "internal_memalign",
                    "dlfree",
                    "__muldc3",
                    "__divdc3")
      define -= "__original_main"
    }
    define
  }
  /**
   * Function Filters the methods related to Wasi using the defined and undefined files in the filter folder
   * @param defined
   * @param list
   * @return
   */
  /*def filterCoverageReport(defined:Set[String], list: List[ModuleCoverageInfo]) : ListBuffer[ModuleCoverageInfo] = {
    val m = new ListBuffer[ModuleCoverageInfo]()
    for(w <- 0 to list.length-1) {
      if (!defined.contains(list(w).methodName))
        m += list(w)
    }
    m
  }*/

  /*def filterCoverageShowMap(defined:Set[String], list: List[ModuleShowMap]) : ListBuffer[ModuleShowMap] = {
    val m = new ListBuffer[ModuleShowMap]()
    for(w <- 0 to list.length-1) {
      if (!defined.contains(list(w).methodName))
        m += list(w)
    }
    m
  } */
}
