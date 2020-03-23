package swam
package impl

import fs2.Chunk.ByteBuffer

/**
  *@author Javier Cabrera-Arteaga on 2020-03-23
  */
sealed class Pointer[T](val offset: Int, val get: (Int) => T, val save: (Int, T) => Unit)
