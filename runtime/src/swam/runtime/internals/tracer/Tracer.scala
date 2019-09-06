package swam
package runtime

import scala.concurrent._
import java.util.concurrent.locks._

import config._

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter

/** Tracers must implement this interface.
  * Also, tracers must provide and implementation of innerTrace method
  * Every trace event will be recorded in background, with the correct invocation order
  */
class Tracer(val conf: EngineConfiguration) {

//  Events must be written in order
  val locker = new ReentrantLock
  val regex = conf.tracer.filter.r

  /** Public method to record an event

    *  $boundaries
    */
  var traceEvent: (String, Any*) => Unit = (name, args) => {}

  /* = (eventName,args) => {
    val time = System.nanoTime() - now
    executeOnBack(() => {
        logger.info(s"${eventName},${time},${group(args:_*)}")
    })
}*/

  if (conf.tracer.handler != HandlerType.None) {
    val logger = Logger.getLogger("swam")
    logger.setLevel(Level.parse(conf.tracer.level))
  }

  def group(args: Any*) = args.mkString(",")

  /** Make a background request on the event record call

    *  $boundaries
    */
  private def executeOnBack(f: () => Unit) = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    Future {
      blocking {
        try {
          locker.lock()
          f()
        } finally {
          locker.unlock()
        }
      }
    }

  }
}

object Tracer {}
