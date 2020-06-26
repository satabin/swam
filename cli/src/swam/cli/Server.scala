package swam
package cli

// Simple server
import java.io._
import java.net._
import java.nio.file.Path

import cats.effect.IO
import swam.optin.coverage.{CoverageListener, CoverageReporter}

import scala.io._

object Server {
  val server = new ServerSocket(9999)

  def listen(
      preparedFunction: () => IO[Unit],
      time: Boolean,
      coverage_out: Option[Path], 
      watOrWasm: Path, 
      coverageListener: CoverageListener[IO], 
      logOrNot: Boolean
    ): Unit = {

    while (true) {
      val s = server.accept()
      val stream_in = new BufferedSource(s.getInputStream()).getLines()
      val stream_out = new PrintStream(s.getOutputStream())

      Main.executeFunction(preparedFunction, time)
      if (logOrNot) {
        CoverageReporter.instCoverage(coverage_out, watOrWasm, coverageListener, logOrNot)
      }

      stream_out.println(stream_in.next())
      stream_out.flush()
      // s.close()
    }
  }
}
