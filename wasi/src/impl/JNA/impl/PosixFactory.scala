package swam
package impl
package JNA
package impl

import swam.impl.JNA.impl.macos.MacOSPOSIX
import swam.impl.JNA.interfaces.POSIX

/**
  *@author Javier Cabrera-Arteaga on 2020-03-30
  */
class PosixFactory {}

object PosixFactory {
  def apply(): POSIX = new MacOSPOSIX(LibCWrapper.run())
}
