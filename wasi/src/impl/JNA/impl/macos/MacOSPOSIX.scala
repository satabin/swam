package swam
package impl
package JNA
package impl
package macos

import swam.impl.JNA.interfaces.{BaseNativePOSIX, FileStat}

/**
  *@author Javier Cabrera-Arteaga on 2020-03-26
  */
class MacOSPOSIX(libc: LibC) extends BaseNativePOSIX(libc) {
  override def allocateStat(): FileStat = new MacOSFileStat()
}
