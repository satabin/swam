package swam
package wasi

/**
    @author Javier Cabrera-Arteaga on 2020-03-23
  */
class ArrayInstance[T](val offset: Int, val size: Int, val objSize: Int, val get: (Int) => T) {

  val values: List[T] = Range(offset, size * objSize + offset, objSize)
    .map(t => {
      get(t)
    })
    .toList

}
