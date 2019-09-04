
import swam.runtime.internals.tracer._

import util._

import utest._



object TracerTests extends TestSuite {

    val tracer = new StdTracer


    val tests = Tests{

        tracer.traceEvent("spush", 123l, 23)
        tracer.traceEvent("spush", 123l, 23)
        tracer.traceEvent("spush", 123l, 24)
        tracer.traceEvent("spush", 123l, 25)
        tracer.traceEvent("spush", 123l, 26)
        tracer.traceEvent("spush", 123l, 27)
    }
  
}
  