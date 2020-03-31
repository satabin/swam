package swam
package impl
package JNA
package helper

import java.lang.reflect.Field

/**
  *@author Javier Cabrera-Arteaga on 2020-03-26
  */
class LibCHelper {

  import java.io.FileDescriptor

  val STDIN = 0
  val STDOUT = 1
  val STDERR = 2
  val helper = ReflectionHelper()

  var fdField: Field = helper.getProtectedField(classOf[FileDescriptor], "fd")
  var handleField: Field = helper.getProtectedField(classOf[FileDescriptor], "handle")

  def chmod(filename: String, mode: Int): Int = ??? //Chmod.chmod(new Nothing(filename), Integer.toOctalString(mode))

  def chown(filename: String, user: Int, group: Int): Int = {
    /*val launcher = new Nothing(handler)
    var chownResult = -1
    var chgrpResult = -1
    try {
      if (user != -1) chownResult = launcher.runAndWait("chown", "" + user, filename)
      if (group != -1) chgrpResult = launcher.runAndWait("chgrp ", "" + user, filename)
    } catch {
      case e: Exception =>
    }
    if (chownResult != -1 && chgrpResult != -1) 0
    else 1*/
    1
  }

  def getfd(descriptor: FileDescriptor): Int = {
    if (descriptor == null || fdField == null) return -1
    try return fdField.getInt(descriptor)
    catch {
      case e: SecurityException        =>
      case e: IllegalArgumentException =>
      case e: IllegalAccessException   =>
    }
    -1
  }

  def gethandle(descriptor: FileDescriptor): Long = {
    if (descriptor == null || handleField == null) return -1
    try return handleField.getLong(descriptor)
    catch {
      case e: SecurityException        =>
      case e: IllegalArgumentException =>
      case e: IllegalAccessException   =>
    }
    -1
  }

  def getlogin: String = System.getProperty("user.name")

  def setpwent = 0

  def endpwent = 0

  def isatty(fd: Int): Int =
    if (fd == STDOUT || fd == STDIN || fd == STDERR) 1
    else 0

  def lstat(path: String, stat: Nothing): Int = {
    /*val file = new Nothing(path)
    if (!file.exists) handler.error(ERRORS.ENOENT, path)
    // FIXME: Bulletproof this or no?
    val jstat = stat.asInstanceOf[Nothing]
    jstat.setup(path)
    // TODO: Add error reporting for cases we can calculate: ENOTDIR, ENAMETOOLONG, ENOENT*/
    // EACCES, ELOOP, EFAULT, EIO
    0
  }

  def mkdir(path: String, mode: Int): Int = {
    /*val dir = new Nothing(path)
    if (!dir.mkdir) return -1
    chmod(path, mode)*/
    0
  }

  def stat(path: String, stat: Nothing): Int = {
    /*val jstat = stat.asInstanceOf[Nothing]
    try {
      val file = new Nothing(path)
      if (!file.exists) handler.error(ERRORS.ENOENT, path)
      jstat.setup(file.getCanonicalPath)
    } catch {
      case e: IOException =>
      // TODO: Throw error when we have problems stat'ing canonicalizing
    }*/
    0
  }

  def symlink(oldpath: String, newpath: String): Int = {
    /*try return new Nothing(handler).runAndWait("ln", "-s", oldpath, newpath)
    catch {
      case e: Exception =>
    }*/
    -1
  }

  def readlink(oldpath: String, buffer: Nothing, length: Int): Int = {
    /*try {
      val baos = new ByteArrayOutputStream
      new Nothing(handler).runAndWait(baos, "readlink", oldpath)
      val bytes = baos.toByteArray
      if (bytes.length > length || bytes.length == 0) return -1
      buffer.put(bytes, 0, bytes.length - 1) // trim off \n

      return buffer.position
    } catch {
      case e: InterruptedException =>
    }*/
    -1
  }

}

object LibCHelper {
  def apply(): LibCHelper = new LibCHelper()
}
