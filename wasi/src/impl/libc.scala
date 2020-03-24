package swam
package wasi

import java.lang.reflect.{InvocationHandler, Method}

import com.sun.jna._

trait libc extends Library {
  def sprintf(buffer: Array[Byte], format: String, arg: Any): Int

  def snprintf(buffer: Array[Byte], size: Int, format: String, arg: Any): Int

  def fstat(path: String, p: com.sun.jna.Pointer): Int

  def fstat(fd: Int, p: com.sun.jna.Pointer): Int

  /*
  def S_ISREG(m: Int): Int // is regular

  def S_ISDIR(m: Int): Int // is dir

  def S_ISCHR(m: Int): Int // is character

  def S_ISBLK(m: Int): Int // is sblink

  def S_ISFIFO(m: Int): Int

  def S_ISLNK(m: Int): Int

  def S_ISSOCK(m: Int): Int*/

}

object libc {
  private var _libc: libc = null

  def errno = Native.getLastError
  def run(): libc = {
    if (_libc == null) {
      _libc = Native.loadLibrary("c", classOf[libc]).asInstanceOf[libc]
    }
    _libc
  }

  def call: libc = run()

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

  /*
    #ifndef S_IFMT
    /* File type */
    #define S_IFMT                   /* [XSI] type of file mask */
    #define S_IFIFO                  /* [XSI] named pipe (fifo) */
    #define                   /* [XSI] character special */
    #define                   /* [XSI] directory */
    #define                   /* [XSI] block special */
    #define                   /* [XSI] regular */
    #define                   /* [XSI] symbolic link */
    #define                  /* [XSI] socket */
    #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
    #define S_IFWHT         0160000         /* OBSOLETE: whiteout */
    #endif

    /* File mode */
    /* Read, write, execute/search by owner */
    #define S_IRWXU         0000700         /* [XSI] RWX mask for owner */
    #define S_IRUSR         0000400         /* [XSI] R for owner */
    #define S_IWUSR         0000200         /* [XSI] W for owner */
    #define S_IXUSR         0000100         /* [XSI] X for owner */
    /* Read, write, execute/search by group */
    #define S_IRWXG         0000070         /* [XSI] RWX mask for group */
    #define S_IRGRP         0000040         /* [XSI] R for group */
    #define S_IWGRP         0000020         /* [XSI] W for group */
    #define S_IXGRP         0000010         /* [XSI] X for group */
    /* Read, write, execute/search by others */
    #define S_IRWXO         0000007         /* [XSI] RWX mask for other */
    #define S_IROTH         0000004         /* [XSI] R for other */
    #define S_IWOTH         0000002         /* [XSI] W for other */
    #define S_IXOTH         0000001         /* [XSI] X for other */

    #define S_ISUID         0004000         /* [XSI] set user id on execution */
    #define S_ISGID         0002000         /* [XSI] set group id on execution */
    #define S_ISVTX         0001000         /* [XSI] directory restrcted delete */

    #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
    #define S_ISTXT         S_ISVTX         /* sticky bit: not supported */
    #define S_IREAD         S_IRUSR         /* backward compatability */
    #define S_IWRITE        S_IWUSR         /* backward compatability */
    #define S_IEXEC         S_IXUSR         /* backward compatability */
    #endif
    #endif  /* !S_IFMT */
 */
}
