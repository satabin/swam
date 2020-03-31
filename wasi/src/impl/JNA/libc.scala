package swam
package impl
package JNA

import java.nio.ByteBuffer

import cats.instances.int
import com.sun.jna._
import swam.impl.JNA.interfaces.{FileStat, NativePassword, Timeval, NativeGroup}

trait LibC extends Library {
  def chmod(filename: String, mode: Int): Int
  def chown(filename: String, user: Int, group: Int): Int
  def fstat(fd: Int, stat: FileStat): Int
  def fstat(fd: String, stat: FileStat): Int
  def fstat64(fd: Int, stat: FileStat): Int
  def getegid(): Int
  def setegid(egid: Int): Int
  def geteuid(): Int
  def seteuid(euid: Int): Int
  def getgid(): Int
  def getlogin(): String
  def setgid(gid: Int): Int
  def getpgid(): Int
  def getpgid(pid: Int): Int
  def setpgid(pid: Int, pgid: Int): Int
  def getpgrp(): Int
  def setpgrp(pid: Int, pgrp: Int): Int
  def getppid(): Int
  def getpid(): Int
  def getpwent(): NativePassword
  def getpwuid(which: Int): NativePassword
  def getpwnam(which: String): NativePassword
  def getgrent(): NativeGroup
  def getgrgid(which: Int): NativeGroup
  def getgrnam(which: String): NativeGroup
  def setpwent(): Int
  def endpwent(): Int
  def setgrent(): Int
  def endgrent(): Int
  def getuid(): Int
  def setsid(): Int
  def setuid(uid: Int): Int
  def kill(pid: Int, signal: Int): Int
  def lchmod(filename: String, mode: Int): Int
  def lchown(filename: String, user: Int, group: Int): Int
  def link(oldpath: String, newpath: String): Int
  def lstat(path: String, stat: FileStat): Int
  def lstat64(path: String, stat: FileStat): Int
  def mkdir(path: String, mode: Int): Int
  def stat(path: String, stat: FileStat): Int
  def stat64(Spath: String, stat: FileStat): Int
  def symlink(oldpath: String, newpath: String): Int
  def readlink(oldpath: String, buffer: ByteBuffer, len: Int): Int
  def umask(mask: Int): Int
  def utimes(path: String, times: Array[Timeval]): Int
  def fork(): Int
  def waitpid(pid: Int, status: Array[Int], options: Int): Int
  def wait(status: Array[Int]): Int
  def getpriority(which: Int, who: Int): Int
  def setpriority(which: Int, who: Int, prio: Int): Int
  def isatty(fd: Int): Int
  def strerror(errnum: Int): String
  def write(fd: Int, bytes: Array[Byte], size: Int): Int
}

object LibCWrapper {
  private var _libc: LibC = null

  def errno = Native.getLastError
  def run(): LibC = {
    if (_libc == null) {
      _libc = Native.loadLibrary("c", classOf[LibC]).asInstanceOf[LibC]
    }
    _libc
  }

  def call: LibC = run()

  val O_ACCMODE = 3
  val O_RDONLY = 0
  val O_WRONLY = 1
  val O_RDWR = 2
  val O_CREAT = 100
  val IOCTL_TRIM = 0x1277
  val SEEK_END = 2

  val S_IFMT = 0xf000
  val S_IFIFO = 0x1000
  val S_IFCHR = 0x2000
  val S_IFDIR = 0x4000
  val S_IFBLK = 0x6000
  val S_IFREG = 0x8000
  val S_IFLNK = 0xa000
  val S_IFSOCK = 0xc000

}
