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
package internals
package interpreter

import org.openjdk.jmh.infra._
import org.openjdk.jmh.annotations._

import scala.util.Random

@State(Scope.Thread)
class StackPerformances_01_Push {

  private var frame: ThreadFrame[Either[Throwable, ?]] = null
  var intValue: Int = 0
  var longValue: Long = 0L
  var floatValue: Float = 0.0f
  var doubleValue: Double = 0.0d
  var labelValue: Label = 0L

  @Setup(Level.Iteration)
  def setupFrame(): Unit = {
    frame = new ThreadFrame[Either[Throwable, ?]](LowConfig, null)
  }

  @Setup(Level.Invocation)
  def setupValues(): Unit = {
    intValue = Random.nextInt()
    longValue = Random.nextLong()
    floatValue = Random.nextFloat()
    doubleValue = Random.nextDouble()
    labelValue = Random.nextLong()
  }

  @TearDown(Level.Invocation)
  def tearDown(): Unit = {
    frame.clearStack()
  }

  @Benchmark @BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
  def push(bh: Blackhole): Unit = {
    bh.consume(frame.pushInt(intValue))
    bh.consume(frame.pushLong(longValue))
    bh.consume(frame.pushFloat(floatValue))
    bh.consume(frame.pushDouble(doubleValue))
  }

}

@State(Scope.Thread)
class StackPerformances_02_Pop {

  private var frame: ThreadFrame[Either[Throwable, ?]] = null

  @Setup(Level.Iteration)
  def setupFrame(): Unit = {
    frame = new ThreadFrame[Either[Throwable, ?]](LowConfig, null)
    frame.pushInt(Random.nextInt())
    frame.pushLong(Random.nextLong())
    frame.pushFloat(Random.nextFloat())
    frame.pushDouble(Random.nextDouble())
  }

  @TearDown(Level.Invocation)
  def tearDown(): Unit = {
    frame.pushInt(Random.nextInt())
    frame.pushLong(Random.nextLong())
    frame.pushFloat(Random.nextFloat())
    frame.pushDouble(Random.nextDouble())
  }

  @Benchmark @BenchmarkMode(Array(Mode.Throughput))
  def pop(bh: Blackhole): Unit = {
    bh.consume(frame.popValue())
    bh.consume(frame.popValue())
    bh.consume(frame.popValue())
    bh.consume(frame.popValue())
  }

}
