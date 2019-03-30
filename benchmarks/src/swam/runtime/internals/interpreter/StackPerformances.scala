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
package high

import org.openjdk.jmh.infra._
import org.openjdk.jmh.annotations._

import cats._
import cats.implicits._

import scala.util.Random

@State(Scope.Thread)
class StackPerformances_01_Push {

  var frame: Frame[Either[Throwable, ?]] = null
  var intValue: Int = 0
  var longValue: Long = 0l
  var floatValue: Float = 0.0f
  var doubleValue: Double = 0.0d
  var labelValue: Label = 0l

  @Setup(Level.Iteration)
  def setupFrame(): Unit = {
    frame = Frame.makeToplevel[Either[Throwable, ?]](null, Config)
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
    frame.stack.clear()
  }

  @Benchmark @BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
  def push(bh: Blackhole): Unit = {
    bh.consume(frame.stack.pushLabel(labelValue))
    bh.consume(frame.stack.pushInt(intValue))
    bh.consume(frame.stack.pushLong(longValue))
    bh.consume(frame.stack.pushFloat(floatValue))
    bh.consume(frame.stack.pushDouble(doubleValue))
  }

}

@State(Scope.Thread)
class StackPerformances_02_Pop {

  var frame: Frame[Either[Throwable, ?]] = null

  @Setup(Level.Iteration)
  def setupFrame(): Unit = {
    frame = Frame.makeToplevel[Either[Throwable, ?]](null, Config)
    frame.stack.pushInt(Random.nextInt())
    frame.stack.pushLong(Random.nextLong())
    frame.stack.pushFloat(Random.nextFloat())
    frame.stack.pushDouble(Random.nextDouble())
    frame.stack.pushLabel(Random.nextLong())
  }

  @TearDown(Level.Invocation)
  def tearDown(): Unit = {
    frame.stack.pushInt(Random.nextInt())
    frame.stack.pushLong(Random.nextLong())
    frame.stack.pushFloat(Random.nextFloat())
    frame.stack.pushDouble(Random.nextDouble())
    frame.stack.pushLabel(Random.nextLong())
  }

  @Benchmark @BenchmarkMode(Array(Mode.Throughput))
  def pop(bh: Blackhole): Unit = {
    bh.consume(frame.stack.popLabel())
    bh.consume(frame.stack.popValue())
    bh.consume(frame.stack.popValue())
    bh.consume(frame.stack.popValue())
    bh.consume(frame.stack.popValue())
  }

}
