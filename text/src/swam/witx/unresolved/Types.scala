package swam
package witx
package unresolved

/**
@author Javier Cabrera-Arteaga on 2020-03-18
  */
sealed trait WitXType {}

case class EnumType(name: Int) extends WitXType;
