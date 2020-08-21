package swam
package cli

// import swam.cli.Server.{parseMessage, randomCoverageFiller, readSocket, serializeMessage, writeSocket}

import scala.collection.mutable.ListBuffer

// Simple server
import java.io._
import java.net.{ServerSocket, Socket}
import java.nio.ByteBuffer
import java.nio.file.Path

import cats.effect.IO
import swam.code_analysis.coverage.CoverageListener
import swam.runtime.{Function, Value}

import com.typesafe.config.ConfigFactory

object Server {
  val conf = ConfigFactory.load()
  val maxQueue = 50000
  val port = conf.getInt("swam.cli.server.port")
  val server = new ServerSocket(port, maxQueue)

  def listen(
      preparedFunction: IO[Function[IO]],
      wasmArgTypes: List[String],
      time: Boolean,
      watOrWasm: Path,
      coverageListener: CoverageListener[IO]
  ): Unit = {
    System.err.println("wasmArgTypes: " + wasmArgTypes)
    val bufferSize = getRequiredBufferSize(wasmArgTypes)
    System.err.println(s"In listen! Required bufferSize: $bufferSize bytes")

    while (true) {
      System.err.println("Waiting for connection!")
      val clientSocket = server.accept()
      // println("Connection accepted!")

      new Thread(
        new ConnectionThread(preparedFunction,
                             wasmArgTypes,
                             time,
                             watOrWasm,
                             coverageListener,
                             clientSocket,
                             bufferSize)
      ).start

    }
  }

  class ConnectionThread(
      preparedFunction: IO[Function[IO]],
      wasmArgTypes: List[String],
      time: Boolean,
      watOrWasm: Path,
      coverageListener: CoverageListener[IO],
      clientSocket: Socket,
      bufferSize: Int
  ) extends Runnable {

    def run(): Unit = {
      val receivedMessage = readSocket(clientSocket, bufferSize)
      if (receivedMessage.length == 0) {
        throw new Exception("Connection broke!")
      }

      System.err.println("Received message!")
      // println(receivedMessage.mkString(" "))

      val argsParsed = parseMessage(receivedMessage, wasmArgTypes, bufferSize)
      //println("Parsed arguments: " + argsParsed)

      var exitCode = 0
      try {
        Main.executeFunction(preparedFunction, argsParsed, time)
        // val result = Main.executeFunction(preparedFunction, argsParsed, time)
        // println(s"Result of calculation: $result")
      } catch {
        // case e: swam.runtime.StackOverflowException => println(e)
        case e: Exception => {
          // writeSocket(clientSocket, "Error: " + e)
          println(e)
          exitCode = 1
        }
      }
      val filledCoverage = coverageListener.pathInfo
      val message = serializeMessage(exitCode, filledCoverage)
      coverageListener.clean()
      try {
        // writeSocket(clientSocket, "Calculation successful! Result: " + result)
        writeSocket(clientSocket, message)
        System.err.println("Sent back message!")
      } catch {
        case e: java.net.SocketException => println(e)
      }
      clientSocket.close()
    }
  }

  def readSocket(socket: Socket, byteLength: Int): Array[Byte] = {
    socket.getInputStream().readNBytes(byteLength)
  }

  // Parses received message to Wasm Input (Int32, Int64, Float32, Float64)
  def parseMessage(input: Array[Byte], argsTypes: List[String], requiredBufferSize: Int): Vector[Value] = {

    val parameterList = new ListBuffer[Value]()
    var byteIndex = 0

    // Just in case message does not have required size:
    // println(s"Length before padding: ${input.length}")
    val paddedInput = input.padTo(requiredBufferSize, 0.toByte)
    // println(s"Length after padding: ${paddedInput.length}")

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

  def writeSocketString(socket: Socket, string: String): Unit = {
    val out: PrintWriter = new PrintWriter(new OutputStreamWriter(socket.getOutputStream))
    out.println(string)
    out.flush()
  }

  def writeSocket(socket: Socket, message: Array[Byte]): Unit = {
    socket.getOutputStream().write(message)
  }

  def randomCoverageFiller(coverage: Array[Byte]): Array[Byte] = {
    val r = scala.util.Random
    for (_ <- 0 until 10) {
      val randomIndex = r.nextInt(coverage.size - 1)
      coverage(randomIndex) = (coverage(randomIndex).toInt + 1).toByte
      // println(s"coverage($randomIndex): ${coverage(randomIndex)}")
    }
    coverage
  }

  def serializeMessage(exitCode: Int, coverage: Array[Byte]): Array[Byte] = {
    exitCode.toByte +: coverage
  }

  // AFL's input length will be varying a lot - we need to know what the required Byte Array size is for
  // our function's input.
  def getRequiredBufferSize(argsTypes: List[String]): Int = {
    var bufferSize = 0
    for (argType <- argsTypes) {
      System.err.println("argType: " + argType)
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
