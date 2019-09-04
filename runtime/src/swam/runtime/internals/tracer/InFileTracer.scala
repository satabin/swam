package swam
package runtime
package internals
package tracer

import config._
import java.io._
import java.text.SimpleDateFormat
import java.util._

class InFileTracer(val conf: EngineConfiguration) extends Tracer{

    
    def group(args: Any*) = args.mkString(",")


    def innerTrace(eventName: String, time: Long, args: Any*) = () => {
        this.singletonTracer.pw.write(s"${eventName},${time},${group(args: _*)}\n")
        this.singletonTracer.pw.flush()
    }

    object singletonTracer{
        val pw = new FileWriter(s"${conf.tracer.path}", true)
    }
    
}


object InFileTracer{

}