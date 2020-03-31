package swam
package impl
package JNA
package interfaces

/**
    @author Javier Cabrera-Arteaga on 2020-03-26
  */
trait Timeval {

  def setTime(timeval: Array[Long]): Unit
}
