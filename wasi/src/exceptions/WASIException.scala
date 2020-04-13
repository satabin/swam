package swam
package wasi

import swam.wasi.Types.errnoEnum

/**
  * @author Javier Cabrera-Arteaga on 2020-04-13
  */
class WASIException(val errno: errnoEnum.Value) extends Exception
