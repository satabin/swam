package swam
package wasi

import cats.effect.IO
import swam.runtime.Memory

/**
  *@author Javier Cabrera-Arteaga on 2020-04-12
  */
object Header {
  def getString(buffer: Memory[IO], offset: Int, length: Int) = {
    val results = new Array[Byte](length)
    val str = String.valueOf(buffer.readBytes(offset, results).unsafeRunSync())
    str
  }
}
