package swam
package runtime
package internals
package interpreter

import config._

object Config extends EngineConfiguration {
  val stack = new StackConfiguration {
    val height = 5
    val callDepth = 1
  }

  val data = new DataConfiguration {
    val onHeap = true
    val hardMax = 0
  }
}
