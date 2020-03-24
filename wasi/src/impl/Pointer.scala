package swam
package wasi

import fs2.Chunk.ByteBuffer

/**
  *@author Javier Cabrera-Arteaga on 2020-03-23
  */
sealed class Pointer[T](val offset: Int, get: (Int) => T, save: (Int, T) => Unit) {

  /**
    *
    * @param i relative position
    * @return
    */
  def _get(i: Int) = get(i + offset)

  /**
    *
    * @param i relative position
    * @param r
    */
  def _set(i: Int, r: T) = save(i + offset, r)

}
