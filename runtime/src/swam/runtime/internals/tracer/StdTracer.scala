package swam
package runtime
package internals
package tracer

class StdTracer extends Tracer{

    def group(args: Any*) = args.mkString(",")

    def traceEvent(args: Any*) = {
        super.executeOnBack(()=> println(s"${Console.MAGENTA}${group(args: _*)},${System.currentTimeMillis()}${Console.WHITE}"))
    }
}

object StdTracer {

}