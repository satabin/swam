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

import config._
import validation._

import cats.effect.IO

import pureconfig.generic.auto._
import pureconfig.module.catseffect.syntax._

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import cats.effect.Blocker
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import pureconfig.ConfigSource

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
@Fork(value = 2, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class MandelbrotPerformances {

  @Param(Array("-0.7436447860"))
  private var x: Double = _

  @Param(Array("0.1318252536"))
  private var y: Double = _

  @Param(Array("0.00029336"))
  private var d: Double = _

  @Param(Array("10000"))
  private var iterations: Int = _

  private var mandelbrot: (Int, Double, Double, Double) => IO[Unit] = _

  private val executor = Executors.newCachedThreadPool()
  private val blocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(executor))

  implicit val cs = IO.contextShift(ExecutionContext.global)

  @Setup
  def setup(): Unit = {
    mandelbrot = (for {
      v <- Validator[IO](blocker)
      conf <- ConfigSource.default.at("swam.runtime").loadF[IO, EngineConfiguration](blocker)
      e = Engine[IO](conf, v, None)
      m <- e.compile(Paths.get("../../../../benchmarks/resources/mandelbrot.wasm"), blocker)
      i <- m.instantiate
      f <- i.exports.typed.function[(Int, Double, Double, Double), Unit]("mandelbrot").map(Function.untupled(_))
    } yield f).unsafeRunSync()
  }

  @TearDown
  def teardown(): Unit = {
    executor.shutdown()
  }

  @Benchmark
  def computeMandelbrot(bh: Blackhole): Unit = {
    bh.consume(mandelbrot(iterations, x, y, d).unsafeRunSync())
  }

}
