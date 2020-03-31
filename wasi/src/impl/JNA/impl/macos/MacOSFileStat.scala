package swam
package impl
package JNA
package impl
package macos

import java.util

import swam.impl.JNA.impl.annotations.PublicJNAField
import swam.impl.JNA.interfaces.BaseNativeFileStat

/**
  * @author Javier Cabrera-Arteaga on 2020-03-26
  */
class MacOSFileStat() extends BaseNativeFileStat {

  @PublicJNAField(0)
  var st_dev: Int = 0

  @PublicJNAField(1)
  var st_ino = 0 // inode's number (ino_t)

  @PublicJNAField(2)
  val st_mode = 0 // inode protection mode (mode_t - uint16)

  @PublicJNAField(3)
  val st_nlink = 0 // number or hard links to the file (nlink_y - uint16)

  @PublicJNAField(4)
  val st_uid = 0 // user-id of owner (uid_t)

  @PublicJNAField(5)
  val st_gid = 0 // group-id of owner (gid_t)

  @PublicJNAField(6)
  val st_rdev = 0 // device type, for special file inode (st_rdev - dev_t)

  @PublicJNAField(7)
  val st_atime = 0 // Time of last access (time_t)

  @PublicJNAField(8)
  val st_atimensec = 0 // Time of last access (nanoseconds)

  @PublicJNAField(9)
  val st_mtime = 0 // Last data modification time (time_t)

  @PublicJNAField(10)
  val st_mtimensec = 0 // Last data modification time (nanoseconds)

  @PublicJNAField(11)
  val st_ctime = 0 // Time of last status change (time_t)

  @PublicJNAField(12)
  val st_ctimensec = 0 // Time of last status change (nanoseconds)

  @PublicJNAField(13)
  val st_size = 0L // file size, in bytes

  @PublicJNAField(14)
  val st_blocks = 0L // blocks allocated for file

  @PublicJNAField(15)
  val st_blksize = 0 // optimal file system I/O ops blocksize

  @PublicJNAField(16)
  val st_flags = 0 // user defined flags for file

  @PublicJNAField(17)
  val st_gen = 0 // file generation number

  @PublicJNAField(18)
  val st_lspare = 0 // RESERVED: DO NOT USE!

  val st_qspare: Array[Long] = null

  override def isExecutableReal(): Boolean = ???

  override def isGroupOwned(): Boolean = ???

  override def isIdentical(other: Nothing): Boolean = ???

  override def isNamedPipe(): Boolean = ???

  override def isOwned(): Boolean = ???

  override def isROwned(): Boolean = ???

  override def isReadable(): Boolean = ???

  override def isReadableReal(): Boolean = ???

  override def isWritable(): Boolean = ???

  override def isWritableReal(): Boolean = ???

  override def isSetgid(): Boolean = ???

  override def isSetuid(): Boolean = ???

  override def mode(): Int = st_mode

  override def mtime(): Long = st_mtime

  override def nlink(): Int = st_nlink

  override def rdev(): Long = st_rdev

  override def uid(): Int = st_uid

  override def atime(): Long = st_atime

  override def blocks(): Long = st_blocks

  override def blockSize(): Long = st_blksize

  override def ctime(): Long = st_ctime

  override def dev(): Long = st_dev

  override def ftype(): String = ???

  override def gid(): Int = st_gid

  override def groupMember(gid: Int): Boolean = ???

  override def ino(): Long = st_ino
}
