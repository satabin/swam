package swam
package cli

import scala.collection.mutable.ListBuffer

// Simple server
import java.io._
import java.net.{ServerSocket, Socket}
import java.nio.ByteBuffer
import java.nio.file.Path

import cats.effect.IO
import swam.optin.coverage.{CoverageListener, CoverageReporter}
import swam.runtime.{Function, Value}

object Server {
  val maxQueue = 50000
  val server = new ServerSocket(9999, maxQueue)

  def listen(
      preparedFunction: IO[Function[IO]],
      wasmArgTypes: List[String],
      time: Boolean,
      coverage_out: Option[Path],
      watOrWasm: Path,
      coverageListener: CoverageListener[IO],
      logOrNot: Boolean
  ): Unit = {

    println("wasmArgTypes: " + wasmArgTypes)
    val bufferSize = getRequiredBufferSize(wasmArgTypes)
    println(s"In listen! Required bufferSize: $bufferSize bytes")

    while (true) {
      println("Waiting for connection!")
      val clientSocket = server.accept()
      println("Connection accepted!")

      val receivedMessage = readSocket(clientSocket)
      println("Received message!")

      val argsParsed = parseMessage(receivedMessage, wasmArgTypes, bufferSize)
      println("Parsed arguments: " + argsParsed)

      // TODO: Forward exit code in return message as byte
      try {
        val result = Main.executeFunction(preparedFunction, argsParsed, time)
        println(s"Result of calculation: $result")
        if (logOrNot) {
          println("Logging now")
          CoverageReporter.instCoverage(coverage_out, watOrWasm, coverageListener, logOrNot)
        }
        writeSocket(clientSocket, "Calculation successful! Result: " + result)
        println("Sent back message!")
      } catch {
        // case e: swam.runtime.StackOverflowException => println(e)
        case e: Exception => {
          println(e)
          writeSocket(clientSocket, "Error: " + e)
        }
      } finally {
        clientSocket.close()
      }
    }
  }

  def readSocket(socket: Socket): Array[Byte] = {

    // Read Strings:
    val reader = new BufferedReader(new InputStreamReader(socket.getInputStream(), "UTF-8"))
    val line = reader.readLine() // reads a line of text
    line.toCharArray.map(_.toByte)

    // Does not recognise new line / EOF
    // LazyList.continually(socket.getInputStream().read).takeWhile(_ != -1).map(_.toByte).toArray

    // Does not recognise new line / EOF
    // socket.getInputStream().readAllBytes()

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

  // Parses received message to Wasm Input (Int32, Int64, Float32, Float64)
  def parseMessage(input: Array[Byte], argsTypes: List[String], requiredBufferSize: Int): Vector[Value] = {

    val parameterList = new ListBuffer[Value]()
    var byteIndex = 0

    // Just in case message does not have required size:
    println(s"Length before padding: ${input.length}")
    val paddedInput = input.padTo(requiredBufferSize, 0.toByte)
    println(s"Length after padding: ${paddedInput.length}")

    for (argType <- argsTypes) {
      argType match {
        case "Int32" => {
          parameterList += Value.Int32(ByteBuffer.wrap(paddedInput.slice(byteIndex, byteIndex + 4)).getInt)
          byteIndex += 4
        }
        case "Int64" => {
          parameterList += Value.Int64(ByteBuffer.wrap(paddedInput.slice(byteIndex, byteIndex + 8)).getLong)
          byteIndex += 8
        }
        case "Float32" => {
          parameterList += Value.Float32(ByteBuffer.wrap(paddedInput.slice(byteIndex, byteIndex + 4)).getFloat)
          byteIndex += 4
        }
        case "Float64" => {
          parameterList += Value.Float64(ByteBuffer.wrap(paddedInput.slice(byteIndex, byteIndex + 8)).getDouble)
          byteIndex += 8
        }
        case unknownType =>
          throw new Exception("Type does not exist for Wasm: " + unknownType)
      }
    }
    parameterList.toVector
  }

  // AFL's input length will be varying a lot - we need to know what the required Byte Array size is for
  // our function's input.
  def getRequiredBufferSize(argsTypes: List[String]): Int = {
    var bufferSize = 0
    for (argType <- argsTypes) {
      println("argType: " + argType)
      argType match {
        case "Int32" =>
          bufferSize += 4
        case "Int64" =>
          bufferSize += 8
        case "Float32" =>
          bufferSize += 4
        case "Float64" =>
          bufferSize += 8
        case unknownType =>
          throw new Exception("Type does not exist for Wasm: " + unknownType)
      }
    }
    bufferSize
  }
}
