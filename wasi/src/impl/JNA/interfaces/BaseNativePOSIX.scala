package swam
package impl
package JNA
package interfaces

import java.io.FileDescriptor
import java.nio.ByteBuffer
import java.nio.charset.Charset

import com.sun.jna.Native
import swam.impl.JNA.helper.LibCHelper

/**
    @author Javier Cabrera-Arteaga on 2020-03-26
  */
abstract class BaseNativePOSIX(val libc: LibC) extends POSIX {

  def allocateStat(): FileStat

  override def chmod(filename: String, mode: Int): Int = libc.chmod(filename, mode)

  override def chown(filename: String, user: Int, group: Int): Int = libc.chown(filename, user, group)

  override def fork: Int = libc.fork()

  override def fstat(descriptor: FileDescriptor): FileStat = {
    val stat = allocateStat()
    val fd: Int = LibCHelper().getfd(descriptor)
    libc.fstat(fd, stat)
    stat
  }

  override def fstat(descriptor: Int): FileStat = {
    val stat = allocateStat()
    libc.fstat(descriptor, stat)
    stat
  }

  override def getegid: Int = libc.getegid()

  override def geteuid: Int = libc.geteuid()

  override def seteuid(euid: Int): Int = libc.seteuid(euid)

  override def getgid: Int = libc.getgid()

  override def getlogin: String = libc.getlogin()

  override def getpgid: Int = libc.getpgid()

  override def getpgid(pid: Int): Int = libc.getpgid()

  override def getpgrp: Int = libc.getpgrp()

  override def getpid: Int = libc.getpid()

  override def getppid: Int = libc.getppid()

  override def getpriority(which: Int, who: Int): Int = libc.getpriority(which, who)

  override def getpwent: NativePassword = libc.getpwent()

  override def getpwuid(which: Int) = libc.getpwuid(which)

  override def getpwnam(which: String) = libc.getpwnam(which)

  override def getgrgid(which: Int) = libc.getgrgid(which)

  override def getgrnam(which: String) = libc.getgrnam(which)

  override def getgrent = libc.getgrent()

  override def endgrent: Int = libc.endgrent()

  override def setgrent: Int = libc.setgrent()

  override def endpwent: Int = libc.endpwent()

  override def setpwent: Int = libc.setpwent()

  override def getuid: Int = libc.getuid()

  override def isatty(descriptor: FileDescriptor): Boolean = libc.isatty(LibCHelper().getfd(descriptor)) != 0

  override def kill(pid: Int, signal: Int): Int = libc.kill(pid, signal)

  override def lchmod(filename: String, mode: Int): Int = libc.lchmod(filename, mode)

  override def lchown(filename: String, user: Int, group: Int): Int = libc.lchown(filename, user, group)

  override def link(oldpath: String, newpath: String): Int = libc.link(oldpath, newpath)

  override def lstat(path: String): FileStat = {
    val stat: FileStat = allocateStat()
    libc.lstat(path, stat)
    stat
  }

  override def mkdir(path: String, mode: Int): Int = libc.mkdir(path, mode)

  override def readlink(path: String): String = {

    val buffer = ByteBuffer.allocate(256)
    val result = libc.readlink(path, buffer, buffer.capacity)

    if (result == -1) return null

    buffer.position(0)
    buffer.limit(result)
    Charset.forName("ASCII").decode(buffer).toString
  }

  override def setsid: Int = libc.setsid()

  override def setgid(gid: Int): Int = libc.setgid(gid)

  override def setegid(egid: Int): Int = libc.setegid(egid)

  override def setpgid(pid: Int, pgid: Int): Int = libc.setpgid(pid, pgid)

  override def setpgrp(pid: Int, pgrp: Int): Int = libc.setpgrp(pid, pgrp)

  override def setpriority(which: Int, who: Int, prio: Int): Int = libc.setpriority(which, who, prio)

  override def setuid(uid: Int): Int = libc.setuid(uid)

  override def stat(path: String): FileStat = {
    val stat = allocateStat()
    libc.stat(path, stat)
    stat
  }

  override def symlink(oldpath: String, newpath: String): Int = libc.symlink(oldpath, newpath)

  override def umask(mask: Int): Int = libc.umask(mask)

  override def utimes(path: String, atimeval: Array[Long], mtimeval: Array[Long]): Int = ??? // TODO

  override def waitpid(pid: Int, status: Array[Int], flags: Int): Int = libc.waitpid(pid, status, flags)

  override def wait(status: Array[Int]): Int = libc.wait(status)

  override def errno: Int = Native.getLastError

  override def errno(value: Int): Unit = Native.setLastError(value);
}
