package swam
package wasi
import java.nio.ByteBuffer

import cats.effect.IO
import swam.runtime.Memory

/**
  * @author Javier Cabrera-Arteaga on 2020-03-23
  */
abstract class WASI_STRUCT {
  def write(offset: Int, mem: Memory[IO])
}
