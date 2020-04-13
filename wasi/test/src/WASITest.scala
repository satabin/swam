import java.io.FileDescriptor
import scala.sys.process._

import utest.{TestSuite, Tests, test}

/**
    @author Javier Cabrera-Arteaga on 2020-04-13
  */
object WASITest extends TestSuite {

  val tests = Tests {
    test("testing_wasi") {
      val r = "sh wasi/test/resources/test_all.sh".!

      if (r > 0)
        System.exit(r)
    }
  }

}
