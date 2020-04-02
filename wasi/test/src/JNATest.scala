package swam
package wasi

import java.io.{File, FileDescriptor, InputStream, PrintStream}
import java.{io, util}
import java.lang.reflect.{Field, Modifier}
import java.nio.file.Paths

import org.jruby.ext.posix._
import utest._

import scala.concurrent.ExecutionContext

class DummyHandler extends POSIXHandler {
  override def error(errors: POSIX.ERRORS, s: Name): Unit = {}

  override def unimplementedError(s: Name): Unit = {}

  override def warn(warning_id: POSIXHandler.WARNING_ID, s: Name, objects: Any*): Unit = {}

  override def isVerbose: Boolean = false

  override def getCurrentWorkingDirectory: File = { new File("/tmp") }

  override def getEnv: Array[Name] = throw new UnsupportedOperationException("Not supported yet.")

  override def getInputStream: InputStream = throw new UnsupportedOperationException("Not supported yet.")

  override def getOutputStream: PrintStream = throw new UnsupportedOperationException("Not supported yet.")

  override def getPID: GlobalIdx = throw new UnsupportedOperationException("Not supported yet.")

  override def getErrorStream: PrintStream = throw new UnsupportedOperationException("Not supported yet.")
}

object JNATest extends TestSuite {

  val tests = Tests {
    test("Testing_stdout") {

      val pos = POSIXFactory.getPOSIX(new DummyHandler(), true)

      val st = pos.fstat(FileDescriptor.in)

      println(st)
    }
  }

}
