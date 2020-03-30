package swam
package impl
package JNA
package interfaces

/**
    @author Javier Cabrera-Arteaga on 2020-03-26
  */
trait FileStat {

  val S_IFIFO = 0x1000 // named pipe (fifo)

  val S_IFCHR = 0x2000 // character special

  val S_IFDIR = 0x4000 // directory

  val S_IFBLK = 0x6000 // block special

  val S_IFREG = 0x8000 // regular

  val S_IFLNK = 0xa000 // symbolic link

  val S_IFSOCK = 0xc000 // socket

  val S_IFMT = 0xf000 // file mask for type checks

  val S_ISUID = 0x800 // set user id on execution

  val S_ISGID = 0x400 // set group id on execution

  val S_ISVTX = 0x200 // save swapped text even after use

  val S_IRUSR = 0x100 // read permission, owner

  val S_IWUSR = 0x80 // write permission, owner

  val S_IXUSR = 0x40 // execute/search permission, owner

  val S_IRGRP = 0x20 // read permission, group

  val S_IWGRP = 0x10 // write permission, group

  val S_IXGRP = 0x8 // execute/search permission, group

  val S_IROTH = 0x4 // read permission, other

  val S_IWOTH = 0x2 // write permission, other

  val S_IXOTH = 0x1 // execute permission, other

  val ALL_READ: Int = S_IRUSR | S_IRGRP | S_IROTH
  val ALL_WRITE: Int = S_IWUSR | S_IWGRP | S_IWOTH
  val S_IXUGO: Int = S_IXUSR | S_IXGRP | S_IXOTH

  def atime(): Long

  def blocks(): Long

  def blockSize(): Long

  def ctime(): Long

  def dev(): Long

  def ftype(): String

  def gid(): Int

  def groupMember(gid: Int): Boolean

  def ino(): Long

  def isBlockDev(): Boolean

  def isCharDev(): Boolean

  def isDirectory(): Boolean

  def isEmpty(): Boolean

  def isExecutable(): Boolean

  def isExecutableReal(): Boolean

  def isFifo(): Boolean

  def isFile(): Boolean

  def isGroupOwned(): Boolean

  def isIdentical(other: Nothing): Boolean

  def isNamedPipe(): Boolean

  def isOwned(): Boolean

  def isROwned(): Boolean

  def isReadable(): Boolean

  def isReadableReal(): Boolean

  def isWritable(): Boolean

  def isWritableReal(): Boolean

  def isSetgid(): Boolean

  def isSetuid(): Boolean

  def isSocket(): Boolean

  def isSticky(): Boolean

  def isSymlink(): Boolean

  // def major(dev: Long): Int

  // def minor(dev: Long): Int

  def mode(): Int

  def mtime(): Long

  def nlink(): Int

  def rdev(): Long

  def st_size(): Long

  def uid(): Int

}
