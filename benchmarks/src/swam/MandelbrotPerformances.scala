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
import formats.DefaultFormatters._

import cats.implicits._
import cats.effect.IO

import pureconfig._
import pureconfig.generic.auto._
import pureconfig.module.squants._
import pureconfig.module.catseffect._

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.nio.file.Paths
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
@Fork(value = 2, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class MandelbrotPerformances {

  @Param(Array("true", "false"))
  private var useLowLevelAsm: Boolean = _

  @Param(Array("-0.7436447860"))
  private var x: Double = _

  @Param(Array("0.1318252536"))
  private var y: Double = _

  @Param(Array("0.00029336"))
  private var d: Double = _

  @Param(Array("10000"))
  private var iterations: Int = _

  private var mandelbrot: exports.EFunction4[Int, Double, Double, Double, Unit, IO] = _

  @Setup
  def setup(): Unit = {
    mandelbrot = (for {
      v <- Validator[IO]
      conf <- loadConfigF[IO, EngineConfiguration]("swam.runtime")
      e = Engine[IO](conf.copy(useLowLevelAsm = useLowLevelAsm), v)
      m <- e.compile(Paths.get("../../../../benchmarks/resources/mandelbrot.wasm"))
      i <- m.instantiate
      f <- i.exports.typed.procedure4[Int, Double, Double, Double]("mandelbrot")
    } yield f).unsafeRunSync()
  }

  @Benchmark
  def computeMandelbrot(bh: Blackhole): Unit = {
    bh.consume(mandelbrot(iterations, x, y, d).unsafeRunSync())
  }

}
