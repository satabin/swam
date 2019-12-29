package swam
package runtime

import java.util.concurrent.locks._

import config._

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.Handler
import java.util.logging.FileHandler
import java.util.logging.SocketHandler
import java.util.logging.ConsoleHandler
import java.util.logging.Formatter
import java.util.logging.LogRecord

/** Tracers must implement this interface.
  * Also, tracers must provide and implementation of innerTrace method
  * Every trace event will be recorded in background, with the correct invocation order
  */
class Tracer(val conf: TraceConfiguration) {

//  Events must be written in order
  val locker = new ReentrantLock
  val regex = conf.filter.r

  /** Public method to record an event

    *  $boundaries
    */
  var traceEvent: (String, Any*) => Unit = (name, args) => {}

  class PureFormatter extends Formatter{

      override def format(x: LogRecord): String = {
        return s"${x.getMillis()},${x.getMessage()}\n"
      }
  }

  def group(args: Any*) = args.mkString(",")

    {
        loggerImpl
    }
  object loggerImpl{
    if (conf.handler != HandlerType.None) {
        val logger = Logger.getLogger("swam")
        logger.setLevel(Level.parse(conf.level))
        
        var handler: Handler = null
    
        conf.handler match {
            case HandlerType.Console => {
                handler = new ConsoleHandler()
            }
            case  HandlerType.File => {
                handler = new FileHandler(s"${conf.fileHandler.folder}/${conf.fileHandler.pattern}",
                conf.fileHandler.append)
            }
            case  HandlerType.Socket => {
                handler = new SocketHandler(conf.socketHandler.host,
                conf.socketHandler.port)
            }
            case  HandlerType.Custom => handler = Class.forName(conf.custom.className).newInstance().asInstanceOf[java.util.logging.Handler]
            // The class must have an empty constructor
            // TODO add custom parameters
        }
    
        // TODO add other formatters support
        handler.setFormatter(new PureFormatter)
    
        logger.setUseParentHandlers(false) // Avoid parent handler
        logger.addHandler(handler)
    
        traceEvent = (name, args) => {
            logger.info(s"${name},${group(args: _*)}")
        }
      }
  }

}

object Tracer {}
