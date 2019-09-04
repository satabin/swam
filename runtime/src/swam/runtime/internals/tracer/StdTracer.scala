package swam
package runtime
package internals
package tracer

import config._

class StdTracer(val conf: EngineConfiguration) extends Tracer{

    def group(args: Any*) = args.mkString(",")

    def innerTrace(eventName: String, args: Any*) = ()=> println(s"${Console.MAGENTA} ${eventName},${group(args: _*)},${System.currentTimeMillis()} ${Console.WHITE}")
    
}
