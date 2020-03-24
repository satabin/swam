package impl

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import com.sun.jna.Native
import swam.wasi.libc

/**
  *@author Javier Cabrera-Arteaga on 2020-03-24
  */
class FileStat(val path: String = "", val fd: Int = -1) {

  val native = libc.run()
  //val r = l.fstat(, 1)
  val p = new com.sun.jna.Pointer(Native.malloc(100))
  val stats: Option[os.StatInfo] = if (!path.isEmpty) Option(os.stat(os.Path(path))) else None
  val r =
    if (!path.isEmpty)
      native.fstat(path, p)
    else
      native.fstat(fd, p)

  val dev: Long = p.getInt(0)
  val ino: Long = p.getLong(2) // long long + 16
  val mode: Int = p.getShort(18)
  val nlink: Int = p.getShort(20)
  val uid: Int = p.getInt(22)
  val gid: Int = p.getInt(26)
  val rdev: Long = p.getInt(30)

  val size: Long = stats match {
    case Some(x) => x.size
    case None    => 0
  } // long long + 16

  val blksize: Long = p.getInt(50)
  val blocks: Long = p.getLong(54) // long long + 16

  val atimeMs: Long = stats match {
    case Some(x) => x.atime.to(TimeUnit.MILLISECONDS)
    case None    => 0
  }

  val mtimeMs: Long = stats match {
    case Some(x) => x.mtime.to(TimeUnit.MILLISECONDS)
    case None    => 0
  }

  val ctimeMs: Long = stats match {
    case Some(x) => x.ctime.to(TimeUnit.MILLISECONDS)
    case None    => 0
  }

  val birthtimeM: Long = 0 // TODO

  def isFile() = (mode & libc.S_IFREG) == libc.S_IFREG
  def isDirectory() = (mode & libc.S_IFDIR) == libc.S_IFDIR
  def isBlockDevice() = (mode & libc.S_IFBLK) == libc.S_IFBLK
  def isCharacterDevice() = (mode & libc.S_IFCHR) == libc.S_IFCHR
  def isSymbolicLink() = (mode & libc.S_IFLNK) == libc.S_IFLNK
  def isFIFO() = (mode & libc.S_IFIFO) == libc.S_IFIFO
  def isSocket() = (mode & libc.S_IFSOCK) == libc.S_IFSOCK

  def write(data: Array[Byte]) = {
    if (!path.isEmpty)
      os.write(os.Path(path), data)
    else {
      fd match {
        case 1 => System.out.write(data, 0, data.length)
        case 2 => System.err.write(data, 0, data.length)
      }
    }
  }

  def read(offset: Int, length: Int): Array[Byte] = {
    if (!path.isEmpty)
      os.read.bytes(os.Path(path), offset, length)
    else {
      val result = new Array[Byte](length)
      fd match {
        case 0 => System.in.read(result, 0, result.length)
        case _ => throw new Exception(s"No std channel $fd")
      }

      result
    }
  }
  // TODO free pointer
}
