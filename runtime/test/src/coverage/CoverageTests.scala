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
package runtime
package trace

import swam._
import runtime.coverage._
import runtime._
import java.nio.file.Paths

import scala.collection.mutable.ListBuffer

object CoverageTests extends TestSuite {

  def runLog(handler: HandlerType) = {
    val conf: TraceConfiguration = new TraceConfiguration(
      handler,
      "\n",
      "*",
      "ALL",
      new TracerFileHandlerCondiguration(
        "log.txt",
        true,
        "."
      ),
      new SocketHanndlerCondiguration("localhost", 8080),
      new CustomTracerConfiguration("unknown")
    )

    val tracer = new JULTracer(conf)

    tracer.traceEvent(EventType.SPush, List("123", "4", "123"))
  }

  def runCoverage(wasmFile: String) = {
    implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.global)

    def instantiate(p: String): Instance[IO] =
      Blocker[IO].use { blocker =>
        for {
          engine <- Engine[IO](blocker)
          tcompiler <- Compiler[IO](blocker)
          m <- engine.compile(tcompiler.stream(Paths.get(p), true, blocker))
          i <- m.instantiate
        } yield i
      }.unsafeRunSync()

    val instance = instantiate(wasmFile)

    val list : ListBuffer[ModuleCoverageInfo] = BuildCoverage(instance)


  }

  val tests = Tests {
    //"console_tracer" - runLog(HandlerType.Console)
    //"file_tracer" - runLog(HandlerType.File)
    // "socket_tracer" - runLog(HandlerType.Socket)

    // "inst 1" - runCoverage('1inst.wasm')
  }
}
