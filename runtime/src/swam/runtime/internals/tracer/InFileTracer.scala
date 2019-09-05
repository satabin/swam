package swam
package runtime
package internals
package tracer

import config._
import java.io._
import java.text.SimpleDateFormat
import java.util._
import java.util.UUID.randomUUID
import scala.collection.mutable.ListBuffer

/** Write events log file
  * Every log file is grouped by the engine execution scope
  */
class InFileTracer(val conf: EngineConfiguration) extends Tracer {


  def traceEvent(eventName: String, args: Any*) = {
    val eventTime = System.nanoTime()
    super.executeOnBack(() => {
      eventName match {
        case regex() => {
          InFileTracerSingleton.write(s"$eventName,${eventTime - now},${group(args: _*)}")
        }
      }
    })
  }

  object InFileTracerSingleton {
    val pw = new FileWriter(s"${conf.tracer.folder}/${conf.tracer.path}", false)

    val buffered: ListBuffer[String] = new ListBuffer()
    val maximum = 1000

    {
      sys.ShutdownHookThread({
        flushBuffer()
      })
    }

    def flushBuffer() {
      for (l <- buffered)
        pw.write(s"${l}\n")

      buffered.clear()
      pw.flush()
    }
    def write(line: String) = {

      buffered += line

      if (buffered.size >= maximum) {
        flushBuffer()
      }
    }
  }
}
