package swam
package runtime
package internals
package tracer

import config._
import java.io._
import java.text.SimpleDateFormat
import java.util._
import java.util.UUID.randomUUID

/** Write events log file
 * Every log file is grouped by the engine execution scope
  */
class InFileTracer(val conf: EngineConfiguration) extends Tracer{

    
    // TODO improve this
    def group(args: Any*) = args.mkString(",")

    def innerTrace(eventName: String, time: Long, args: Any*) = () => {
        val path = conf.tracer.path
        val pw = new FileWriter(s"${path}", true)

        pw.write(s"${eventName},${time},${group(args: _*)}\n")
        pw.close()
    }

}


object InFileTracer{

}