package swam
package runtime
package internals
package tracer

import config._

class StdTracer(val conf: EngineConfiguration) extends Tracer{

    def group(args: Any*) = args.mkString(",")

    def innerTrace(eventName: String, time: Long, args: Any*) = ()=> println(s"${Console.MAGENTA} ${eventName},${time},${group(args: _*)} ${Console.WHITE}")
    
}


object StdTracer{
    
}