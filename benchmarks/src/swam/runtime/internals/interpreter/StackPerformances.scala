package swam
package runtime
package internals
package interpreter

import org.openjdk.jmh.infra._
import org.openjdk.jmh.annotations._

import cats._

import scala.util.Random

@State(Scope.Thread)
class StackPerformances_01_Push {

  var frame: Frame[Id] = null
  var intValue: Int = 0
  var longValue: Long = 0l
  var floatValue: Float = 0.0f
  var doubleValue: Double = 0.0d
  var labelValue: Label = 0l

  @Setup(Level.Iteration)
  def setupFrame(): Unit = {
    frame =  Frame.makeToplevel[Id](null, Config)
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
