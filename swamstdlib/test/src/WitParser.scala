package swam
package stdlib

import utest.{TestSuite, Tests, test}

object WitParser extends TestSuite {

  val tests = Tests {
    test("console_tracer") { println(12) }
  }

}
