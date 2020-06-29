package swam
package cli

// Simple server
import java.io._
import java.net.{ServerSocket, Socket}
import java.nio.file.Path

import cats.effect.IO
import swam.optin.coverage.{CoverageListener, CoverageReporter}

object Server {
  val maxQueue = 50000
  val server = new ServerSocket(9999, maxQueue)

  def readSocket(socket: Socket): String = {

    val reader = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    val line = reader.readLine() // reads a line of text
    line

    // val bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream))
    // var request = ""
    // var line = ""
    // do {
    //   println("line: " + line)
    //   line = bufferedReader.readLine()
    //   println("line: " + line)
    //   if (line == null) {
    //     println("Stream terminated")
    //     println("request: " + request)
    //     return request
    //   }
    //   request += line + "\n"
    // } while (line != "")
    // println("request: " + request)
    // request
  }

  def writeSocket(socket: Socket, string: String): Unit = {
    val out: PrintWriter = new PrintWriter(new OutputStreamWriter(socket.getOutputStream))
    out.println(string)
    out.flush()

    // val writer = new PrintWriter(clientSocket.getOutputStream(), true)

    // val writer = new PrintStream(s.getOutputStream())
    // writer.flush()
  }

  def listen(
      preparedFunction: () => IO[Unit],
      time: Boolean,
      coverage_out: Option[Path],
      watOrWasm: Path,
      coverageListener: CoverageListener[IO],
      logOrNot: Boolean
  ): Unit = {

    println("In listen!")

    while (true) {
      val clientSocket = server.accept()
      println("Accepted!")

      val receivedMessage = readSocket(clientSocket)
      println("receivedMessage: " + receivedMessage)

      Main.executeFunction(preparedFunction, time)
      if (logOrNot) {
        println("Logging now")
        CoverageReporter.instCoverage(coverage_out, watOrWasm, coverageListener, logOrNot)
      }

      writeSocket(clientSocket, "Got the message!")

      println("Finished writeSocket")

      clientSocket.close()
    }
  }
}
