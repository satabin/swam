package swam
package runtime
package internals
package tracer

import config._
import java.io._
import java.text.SimpleDateFormat
import java.util._
import java.util.UUID.randomUUID

class InFileTracer(val conf: EngineConfiguration) extends Tracer{

    
    def group(args: Any*) = args.mkString(",")

    val pw = new FileWriter(s"${conf.tracer.path}", false)

    def innerTrace(eventName: String, time: Long, args: Any*) = () => {
        pw.write(s"${eventName},${time},${group(args: _*)}\n")
        pw.flush()
    }

}


object InFileTracer{

}