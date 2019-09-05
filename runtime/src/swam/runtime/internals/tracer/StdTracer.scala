package swam
package runtime
package internals
package tracer

import config._

/** Print events to console

  */
class StdTracer(val conf: EngineConfiguration) extends Tracer {

  def traceEvent(eventName: String, args: Any*) = {
    val eventTime = System.nanoTime()
    // Filtering usong trace options
    super.executeOnBack(() => {
      eventName match {
        case regex(_*) =>
          println(s"${Console.MAGENTA} $eventName, ${eventTime - now} , ${group(args: _*)} ${Console.WHITE} ")
      }
    })
  }

}

object StdTracer {}
