/*
 * Copyright 2018 Lucas Satabin
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
package text

import parser._
import runtime._
import config._

import swam.test.util._

import utest._

import better.files._

import fastparse._

import cats.effect._

import scala.concurrent.ExecutionContext

class CustomHandler extends java.util.logging.Handler {
  def close(): Unit = {}
  def flush(): Unit = {}
  def publish(log: java.util.logging.LogRecord): Unit = {
    println(s"From custom handler :)  ${log.getMessage()}")
  }
}
object TracerTests extends TestSuite {

  def runLog(handler: HandlerType) = {
    val conf: TraceConfiguration = new TraceConfiguration(
      handler,
      "",
      "ALL",
      new TracerFileHandlerCondiguration(
        "test-log.txt",
        true,
        "."
      ),
      new SocketHanndlerCondiguration("localhost", 8080),
      new CustomTracerConfiguration("swam.text.CustomHandler")
    )

    val tracer = new Tracer(conf)

    tracer.traceEvent("testEvent", 123, 4, 123)
  }

  val tests = Tests {
    "console_tracer" - runLog(HandlerType.Console)
    "file_tracer" - runLog(HandlerType.File)
    "socket_tracer" - runLog(HandlerType.Socket)
    "custom_tracer" - runLog(HandlerType.Custom)
  }

}
