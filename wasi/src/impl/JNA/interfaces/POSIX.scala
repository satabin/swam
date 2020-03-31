package swam
package impl
package JNA
package interfaces

/**
    @author Javier Cabrera-Arteaga on 2020-03-26
  */
trait POSIX {

  import java.io.FileDescriptor
  import java.io.IOException

  object ERRORS extends Enumeration {
    type ERRORS = Value
    val ENOENT, ENOTDIR, ENAMETOOLONG, EACCESS, ELOOP, EFAULT, EIO, EBADF = Value
  }

  def chmod(filename: String, mode: Int): Int

  def chown(filename: String, user: Int, group: Int): Int

  def fork: Int

  def fstat(descriptor: FileDescriptor): FileStat

  def getegid: Int

  def geteuid: Int

  def seteuid(euid: Int): Int

  def getgid: Int

  def getlogin: String

  def getpgid: Int

  def getpgid(pid: Int): Int

  def getpgrp: Int

  def getpid: Int

  def getppid: Int

  def getpriority(which: Int, who: Int): Int

  def getpwent: NativePassword

  def getpwuid(which: Int): NativePassword

  def getpwnam(which: String): NativePassword

  def getgrgid(which: Int): NativeGroup

  def getgrnam(which: String): NativeGroup

  def getgrent: NativeGroup

  def endgrent: Int

  def setgrent: Int

  def endpwent: Int

  def setpwent: Int

  def getuid: Int

  def isatty(descriptor: FileDescriptor): Boolean

  def kill(pid: Int, signal: Int): Int

  def lchmod(filename: String, mode: Int): Int

  def lchown(filename: String, user: Int, group: Int): Int

  def link(oldpath: String, newpath: String): Int

  def lstat(path: String): FileStat

  def mkdir(path: String, mode: Int): Int

  def readlink(path: String): String

  def setsid: Int

  def setgid(gid: Int): Int

  def setegid(egid: Int): Int

  def setpgid(pid: Int, pgid: Int): Int

  def setpgrp(pid: Int, pgrp: Int): Int

  def setpriority(which: Int, who: Int, prio: Int): Int

  def setuid(uid: Int): Int

  def stat(path: String): FileStat

  def fstat(fd: Int): FileStat

  def symlink(oldpath: String, newpath: String): Int

  def umask(mask: Int): Int

  def utimes(path: String, atimeval: Array[Long], mtimeval: Array[Long]): Int

  def waitpid(pid: Int, status: Array[Int], flags: Int): Int

  def wait(status: Array[Int]): Int

  def errno: Int

  def errno(value: Int): Unit

  def strerror(errnum: Int): String

  def write(fd: Int, bytes: Array[Byte], size: Int): Int

}
