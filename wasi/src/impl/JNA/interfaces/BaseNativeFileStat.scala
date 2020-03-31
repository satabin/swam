package swam
package impl
package JNA
package interfaces

import com.sun.jna.Structure
import swam.impl.JNA.impl.ScalaStructure
import swam.impl.JNA.interfaces.FileStat

/**
  *@author Javier Cabrera-Arteaga on 2020-03-26
  */
abstract class BaseNativeFileStat extends ScalaStructure with FileStat {
  def isBlockDev() = (mode() & S_IFMT) == S_IFBLK

  def isCharDev() = (mode() & S_IFMT) == S_IFCHR

  def isDirectory() = (mode() & S_IFMT) == S_IFDIR

  def isEmpty() = st_size() == 0

  def isExecutable() = {
    if (isOwned()) (mode() & S_IXUSR) != 0
    if (isGroupOwned()) (mode() & S_IXGRP) != 0
    (mode() & S_IXOTH) != 0
  }

  def isFile() = (mode() & S_IFMT) == S_IFREG

  def isFifo() = (mode() & S_IFMT) == S_IFIFO

  def isSocket() = (mode() & S_IFMT) == S_IFSOCK

  def isSticky() = (mode() & S_ISVTX) != 0

  def isSymlink() = (mode() & S_IFMT) == S_IFLNK

}
