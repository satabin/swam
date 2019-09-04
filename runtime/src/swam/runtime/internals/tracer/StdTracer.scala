package swam
package runtime
package internals
package tracer

class StdTracer extends Tracer{

    def group(args: Any*) = args.mkString(",")

    def traceEvent(args: Any*) = {
        super.executeOnBack(()=> println(s"${group(args: _*)},${System.currentTimeMillis()}"))
    }
}