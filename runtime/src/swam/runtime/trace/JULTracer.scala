/*
 * Copyright 2019 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam
package runtime
package trace

import enumeratum._
import pureconfig.error._
import java.util.logging._

import cats.implicits._
import cats.effect._

import pureconfig._
import pureconfig.generic.auto._
import pureconfig.module.catseffect.syntax._
import pureconfig.module.enumeratum._

/** A tracer based on [[https://docs.oracle.com/en/java/javase/13/docs/api/java.logging/java/util/logging/package-summary.html java.util.logging]]. */
class JULTracer(conf: TraceConfiguration, formatter: Formatter = PureFormatter) extends Tracer {

  val logger = Logger.getLogger("swam")
  logger.setLevel(Level.parse(conf.level))
  logger.setUseParentHandlers(false) // Avoid parent handler

  val handler =
    conf.handler match {
      case HandlerType.Console =>
        new ConsoleHandler()
      case HandlerType.File =>
        new FileHandler(s"${conf.fileHandler.folder}/${conf.fileHandler.pattern}", conf.fileHandler.append)
      case HandlerType.Socket =>
        new SocketHandler(conf.socketHandler.host, conf.socketHandler.port)
      case HandlerType.Custom =>
        Class
          .forName(conf.custom.className)
          .getDeclaredConstructor()
          .newInstance()
          .asInstanceOf[java.util.logging.Handler]
    }

  // TODO add other formatters support
  handler.setFormatter(formatter)
  logger.addHandler(handler)

  override def filter(tpe: EventType, args: List[String], frame: StackFrame): Boolean =
    conf.filter.equals("*") || tpe.entryName.matches(conf.filter)

  def traceEvent(tpe: EventType, args: List[String]): Unit =
    logger.info(s"${tpe.entryName},${args.mkString(",")}${conf.separator}")

}

object JULTracer {
  def apply[F[_]: Sync: ContextShift](blocker: Blocker,
                                      traceFolder: String,
                                      traceNamePattern: String,
                                      formatter: Formatter = PureFormatter,
                                      filter: String = "*"): F[JULTracer] = {

    for {
      default <- ConfigSource.default.at("swam.runtime.tracer").loadF[F, TraceConfiguration](blocker)
    } yield new JULTracer(
      default.copy(filter = filter, fileHandler = default.fileHandler.copy(pattern = traceNamePattern)),
      formatter)

  }
}

private object PureFormatter extends Formatter {

  override def format(x: LogRecord): String =
    s"${x.getMillis()},${x.getMessage()}"

}

case class TraceConfiguration(handler: HandlerType,
                              separator: String,
                              filter: String,
                              level: String,
                              fileHandler: TracerFileHandlerCondiguration,
                              socketHandler: SocketHanndlerCondiguration,
                              custom: CustomTracerConfiguration)

case class TracerFileHandlerCondiguration(var pattern: String, append: Boolean, folder: String);
case class SocketHanndlerCondiguration(host: String, port: Int);
case class CustomTracerConfiguration(className: String)

sealed trait HandlerType extends EnumEntry with EnumEntry.Lowercase

object HandlerType extends Enum[HandlerType] {

  def values = findValues

  case object Console extends HandlerType
  case object File extends HandlerType
  case object Custom extends HandlerType
  case object Socket extends HandlerType

  implicit object configReader extends ConfigReader[HandlerType] {
    def from(cur: ConfigCursor): ConfigReader.Result[HandlerType] =
      cur.asString.flatMap(s =>
        HandlerType
          .withNameOption(s)
          .fold[ConfigReader.Result[HandlerType]](
            Left(ConfigReaderFailures(ConvertFailure(CannotConvert(s, "HandlerType", "unknown handler type"), cur))))(
            Right(_)))
  }

}
