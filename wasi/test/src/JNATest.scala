package swam
package wasi

import java.{io, util}
import java.lang.reflect.{Field, Modifier}
import java.nio.file.Paths

import swam.impl.JNA.LibCWrapper
import utest._
import swam.impl.JNA.impl.PosixFactory

import scala.concurrent.ExecutionContext

object JNATest extends TestSuite {

  val tests = Tests {
    test("Testing_stdout") {
      val st = PosixFactory()
      val stat = st.fstat(1)
      assert(stat.isFifo())
    }

    test("Testing regular file") {
      val st = PosixFactory()

      val stat = st.stat("generator/resources/wasi_witx/get.sh")

      val err = st.errno

      if (err > 0)
        println(st.strerror(err))
      assert(err == 0)

      assert(stat.isFile())
    }

    test("Testing writing") {
      val libc = LibCWrapper.run()

      libc.write(1, Array[Byte](3, 48, 3, 4), 3)
    }
  }

}
