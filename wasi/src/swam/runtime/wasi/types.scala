/*
 * Copyright 2020 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam
package runtime
package wasi

import formats._

import cats.MonadError
import cats.implicits._

import enumeratum._
import enumeratum.values._

sealed abstract class Clockid(val value: Int) extends IntEnumEntry
object Clockid extends IntEnum[Clockid] {
  case object Realtime extends Clockid(0)
  case object Monotonic extends Clockid(1)
  case object ProcessCputimeId extends Clockid(2)
  case object ThreadCputimeId extends Clockid(3)
  def values: IndexedSeq[Clockid] = findValues

  implicit def reader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Clockid] =
    new SimpleValueReader[F, Clockid] {
      def read(v: Value): F[Clockid] =
        v match {
          case Value.Int32(i) => withValueOpt(i).liftTo[F](new ConversionException(s"unknown clockid $i"))
          case _              => F.raiseError(new ConversionException(s"expected i32 but got ${v.tpe}"))
        }
      val swamType: ValType = ValType.I32
    }
}

sealed abstract class Errno(val value: Short) extends ShortEnumEntry
object Errno extends ShortEnum[Errno] {

  /**  No error occurred. System call completed successfully. */
  case object Success extends Errno(0)

  /**  Argument list too long. */
  case object Toobig extends Errno(1)

  /**  Permission denied. */
  case object Acces extends Errno(2)

  /**  Address in use. */
  case object Addrinuse extends Errno(3)

  /**  Address not available. */
  case object Addrnotavail extends Errno(4)

  /**  Address family not supported. */
  case object Afnosupport extends Errno(5)

  /**  Resource unavailable, or operation would block. */
  case object Again extends Errno(6)

  /**  Connection already in progress. */
  case object Already extends Errno(7)

  /**  Bad file descriptor. */
  case object Badf extends Errno(8)

  /**  Bad message. */
  case object Badmsg extends Errno(9)

  /**  Device or resource busy. */
  case object Busy extends Errno(10)

  /**  Operation canceled. */
  case object Canceled extends Errno(11)

  /**  No child processes. */
  case object Child extends Errno(12)

  /**  Connection aborted. */
  case object Connaborted extends Errno(13)

  /**  Connection refused. */
  case object Connrefused extends Errno(14)

  /**  Connection reset. */
  case object Connreset extends Errno(15)

  /**  Resource deadlock would occur. */
  case object Deadlk extends Errno(16)

  /**  Destination address required. */
  case object Destaddrreq extends Errno(17)

  /**  Mathematics argument out of domain of function. */
  case object Dom extends Errno(18)

  /**  Reserved. */
  case object Dquot extends Errno(19)

  /**  File exists. */
  case object Exist extends Errno(20)

  /**  Bad address. */
  case object Fault extends Errno(21)

  /**  File too large. */
  case object Fbig extends Errno(22)

  /**  Host is unreachable. */
  case object Hostunreach extends Errno(23)

  /**  Identifier removed. */
  case object Idrm extends Errno(24)

  /**  Illegal byte sequence. */
  case object Ilseq extends Errno(25)

  /**  Operation in progress. */
  case object Inprogress extends Errno(26)

  /**  Interrupted function. */
  case object Intr extends Errno(27)

  /**  Invalid argument. */
  case object Inval extends Errno(28)

  /**  I/O error. */
  case object Io extends Errno(29)

  /**  Socket is connected. */
  case object Isconn extends Errno(30)

  /**  Is a directory. */
  case object Isdir extends Errno(31)

  /**  Too many levels of symbolic links. */
  case object Loop extends Errno(32)

  /**  File descriptor value too large. */
  case object Mfile extends Errno(33)

  /**  Too many links. */
  case object Mlink extends Errno(34)

  /**  Message too large. */
  case object Msgsize extends Errno(35)

  /**  Reserved. */
  case object Multihop extends Errno(36)

  /**  Filename too long. */
  case object Nametoolong extends Errno(37)

  /**  Network is down. */
  case object Netdown extends Errno(38)

  /**  Connection aborted by network. */
  case object Netreset extends Errno(39)

  /**  Network unreachable. */
  case object Netunreach extends Errno(40)

  /**  Too many files open in system. */
  case object Nfile extends Errno(41)

  /**  No buffer space available. */
  case object Nobufs extends Errno(42)

  /**  No such device. */
  case object Nodev extends Errno(43)

  /**  No such file or directory. */
  case object Noent extends Errno(44)

  /**  Executable file format error. */
  case object Noexec extends Errno(45)

  /**  No locks available. */
  case object Nolck extends Errno(46)

  /**  Reserved. */
  case object Nolink extends Errno(47)

  /**  Not enough space. */
  case object Nomem extends Errno(48)

  /**  No message of the desired type. */
  case object Nomsg extends Errno(49)

  /**  Protocol not available. */
  case object Noprotoopt extends Errno(50)

  /**  No space left on device. */
  case object Nospc extends Errno(51)

  /**  Function not supported. */
  case object Nosys extends Errno(52)

  /**  The socket is not connected. */
  case object Notconn extends Errno(53)

  /**  Not a directory or a symbolic link to a directory. */
  case object Notdir extends Errno(54)

  /**  Directory not empty. */
  case object Notempty extends Errno(55)

  /**  State not recoverable. */
  case object Notrecoverable extends Errno(56)

  /**  Not a socket. */
  case object Notsock extends Errno(57)

  /**  Not supported, or operation not supported on socket. */
  case object Notsup extends Errno(58)

  /**  Inappropriate I/O control operation. */
  case object Notty extends Errno(59)

  /**  No such device or address. */
  case object Nxio extends Errno(60)

  /**  Value too large to be stored in data type. */
  case object Overflow extends Errno(61)

  /**  Previous owner died. */
  case object Ownerdead extends Errno(62)

  /**  Operation not permitted. */
  case object Perm extends Errno(63)

  /**  Broken pipe. */
  case object Pipe extends Errno(64)

  /**  Protocol error. */
  case object Proto extends Errno(65)

  /**  Protocol not supported. */
  case object Protonosupport extends Errno(66)

  /**  Protocol wrong type for socket. */
  case object Prototype extends Errno(67)

  /**  Result too large. */
  case object Range extends Errno(68)

  /**  Read-only file system. */
  case object Rofs extends Errno(69)

  /**  Invalid seek. */
  case object Spipe extends Errno(70)

  /**  No such process. */
  case object Srch extends Errno(71)

  /**  Reserved. */
  case object Stale extends Errno(72)

  /**  Connection timed out. */
  case object Timedout extends Errno(73)

  /**  Text file busy. */
  case object Txtbsy extends Errno(74)

  /**  Cross-device link. */
  case object Xdev extends Errno(75)

  /**  Extension: Capabilities insufficient. */
  case object Notcapable extends Errno(76)

  def values: IndexedSeq[Errno] = findValues

  implicit def writer[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Errno] =
    new SimpleValueWriter[F, Errno] {
      def write(v: Errno): Value = Value.Int32(v.value)
      val swamType: ValType = ValType.I32
    }
}

object Rights {
  val FdDatasync: Rights = 1L << 0
  val FdRead: Rights = 1L << 1
  val FdSeek: Rights = 1L << 2
  val FdFdstatSetFlags: Rights = 1L << 3
  val FdSync: Rights = 1L << 4
  val FdTell: Rights = 1L << 5
  val FdWrite: Rights = 1L << 6
  val FdAdvise: Rights = 1L << 7
  val FdAllocate: Rights = 1L << 8
  val PathCreateDirectory: Rights = 1L << 9
  val PathCreateFile: Rights = 1L << 10
  val PathLinkSource: Rights = 1L << 11
  val PathLinkTarget: Rights = 1L << 12
  val PathOpen: Rights = 1L << 13
  val FdReaddir: Rights = 1L << 14
  val PathReadlink: Rights = 1L << 15
  val PathRenameSource: Rights = 1L << 16
  val PathRenameTarget: Rights = 1L << 17
  val PathFilestatGet: Rights = 1L << 18
  val PathFilestatSetSize: Rights = 1L << 19
  val PathFilestatSetTimes: Rights = 1L << 20
  val FdFilestatGet: Rights = 1L << 21
  val FdFilestatSetSize: Rights = 1L << 22
  val FdFilestatSetTimes: Rights = 1L << 23
  val PathSymlink: Rights = 1L << 24
  val PathRemoveDirectory: Rights = 1L << 25
  val PathUnlinkFile: Rights = 1L << 26
  val PollFdReadwrite: Rights = 1L << 27
  val SockShutdown: Rights = 1L << 28

  val all: List[(Rights, String)] =
    List(
      FdDatasync -> "FdDatasync",
      FdRead -> "FdRead",
      FdSeek -> "FdSeek",
      FdFdstatSetFlags -> "FdFdstatSetFlags",
      FdSync -> "FdSync",
      FdTell -> "FdTell",
      FdWrite -> "FdWrite",
      FdAdvise -> "FdAdvise",
      FdAllocate -> "FdAllocate",
      PathCreateDirectory -> "PathCreateDirectory",
      PathCreateFile -> "PathCreateFile",
      PathLinkSource -> "PathLinkSource",
      PathLinkTarget -> "PathLinkTarget",
      PathOpen -> "PathOpen",
      FdReaddir -> "FdReaddir",
      PathReadlink -> "PathReadlink",
      PathRenameSource -> "PathRenameSource",
      PathRenameTarget -> "PathRenameTarget",
      PathFilestatGet -> "PathFilestatGet",
      PathFilestatSetSize -> "PathFilestatSetSize",
      PathFilestatSetTimes -> "PathFilestatSetTimes",
      FdFilestatGet -> "FdFilestatGet",
      FdFilestatSetSize -> "FdFilestatSetSize",
      FdFilestatSetTimes -> "FdFilestatSetTimes",
      PathSymlink -> "PathSymlink",
      PathRemoveDirectory -> "PathRemoveDirectory",
      PathUnlinkFile -> "PathUnlinkFile",
      PollFdReadwrite -> "PollFdReadwrite",
      SockShutdown -> "SockShutdown"
    )

  val allMask = all.foldLeft(0L)((acc, r) => acc | r._1)

  def listRights(rights: Rights): List[String] =
    all.foldLeft(List.empty[String]) {
      case (acc, (right, name)) =>
        if ((rights & right) == 0)
          acc
        else
          name :: acc
    }
}

sealed abstract class Advice(val value: Byte) extends ByteEnumEntry
object Advice extends ByteEnum[Advice] {

  /** The application has no advice to give on its behavior with respect to the specified data. */
  case object Normal extends Advice(0)

  /** The application expects to access the specified data sequentially from lower offsets to higher offsets. */
  case object Sequential extends Advice(1)

  /** The application expects to access the specified data in a random order. */
  case object Random extends Advice(2)

  /** The application expects to access the specified data in the near future. */
  case object Willneed extends Advice(3)

  /** The application expects that it will not access the specified data in the near future. */
  case object Dontneed extends Advice(4)

  /** The application expects to access the specified data once and then not reuse it thereafter. */
  case object Noreuse extends Advice(5)

  def values: IndexedSeq[Advice] = findValues

  implicit def reader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Advice] =
    new SimpleValueReader[F, Advice] {
      def read(v: Value): F[Advice] =
        v match {
          case Value.Int32(i) => withValueOpt(i.toByte).liftTo[F](new ConversionException(s"unknown advice $i"))
          case _              => F.raiseError(new ConversionException(s"expected i32 but got ${v.tpe}"))
        }
      val swamType: ValType = ValType.I32
    }
}

sealed abstract class Filetype(val value: Byte) extends ByteEnumEntry
object Filetype extends ByteEnum[Filetype] {
  case object Unknown extends Filetype(0)
  case object BlockDevice extends Filetype(1)
  case object CharacterDevice extends Filetype(2)
  case object Directory extends Filetype(3)
  case object RegularFile extends Filetype(4)
  case object SocketDgram extends Filetype(5)
  case object SocketStream extends Filetype(6)
  case object SymbolicLink extends Filetype(7)

  def values: IndexedSeq[Filetype] = findValues

  implicit def writer[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueWriter[F, Filetype] =
    new SimpleValueWriter[F, Filetype] {
      def write(v: Filetype): Value = Value.Int32(v.value)
      val swamType: ValType = ValType.I32
    }
}

object FdFlags {
  val Append: FdFlags = 0x0001
  val Dsync: FdFlags = 0x0002
  val Nonblock: FdFlags = 0x0004
  val Rsync: FdFlags = 0x0008
  val Sync: FdFlags = 0x0010

  def hasRights(fdflags: FdFlags, rights: Rights): Boolean =
    if ((fdflags & Rsync) == Rsync && (rights & Rights.FdSync) == 0)
      false
    else if ((fdflags & Dsync) == Dsync && (rights & Rights.FdSync) == 0)
      false
    else if ((fdflags & Dsync) == Dsync && (rights & Rights.FdDatasync) == 0)
      false
    else
      true

}

object Lookupflags {
  val SymlinkFollow: Lookupflags = 0x00000001
}

object Oflags {
  val Creat: Oflags = 0x0001
  val Directory: Oflags = 0x0002
  val Excl: Oflags = 0x0004
  val Trunc: Oflags = 0x0008

  def hasRights(oflags: Oflags, rights: Rights): Boolean =
    if ((oflags & Creat) == Creat && (rights & Rights.PathCreateFile) == 0)
      false
    else if ((oflags & Trunc) == Trunc && (rights & Rights.PathFilestatSetSize) == 0)
      false
    else
      true

}

object Fstflags {
  val Atim: Fstflags = 0x0001
  val AtimNow: Fstflags = 0x0002
  val Mtim: Fstflags = 0x0004
  val MtimNow: Fstflags = 0x0008
}

sealed abstract class Whence(val value: Byte) extends ByteEnumEntry
object Whence extends ByteEnum[Whence] {
  case object Set extends Whence(0)
  case object Cur extends Whence(1)
  case object End extends Whence(2)

  def values: IndexedSeq[Whence] = findValues

  implicit def reader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Whence] =
    new SimpleValueReader[F, Whence] {
      def read(v: Value): F[Whence] =
        v match {
          case Value.Int32(i) => withValueOpt(i.toByte).liftTo[F](new ConversionException(s"unknown whence $i"))
          case _              => F.raiseError(new ConversionException(s"expected i32 but got ${v.tpe}"))
        }
      val swamType: ValType = ValType.I32
    }
}

sealed abstract class Signal(val value: Byte) extends ByteEnumEntry with EnumEntry {
  override def entryName: String = stableEntryName

  private[this] lazy val stableEntryName: String = s"SIG${super.entryName.toUpperCase}"
}
object Signal extends ByteEnum[Signal] {
  // No signal. Note that POSIX has special semantics for `kill(pid, 0)`,
  // so this value is reserved.
  case object None extends Signal(0)
  // Hangup.
  // Action: Terminates the process.
  case object Hup extends Signal(1)
  // Terminate interrupt signal.
  // Action: Terminates the process.
  case object Int extends Signal(2)
  // Terminal quit signal.
  // Action: Terminates the process.
  case object Quit extends Signal(3)
  // Illegal instruction.
  // Action: Terminates the process.
  case object Ill extends Signal(4)
  // Trace/breakpoint trap.
  // Action: Terminates the process.
  case object Trap extends Signal(5)
  // Process abort signal.
  // Action: Terminates the process.
  case object Abrt extends Signal(6)
  // Access to an undefined portion of a memory object.
  // Action: Terminates the process.
  case object Bus extends Signal(7)
  // Erroneous arithmetic operation.
  // Action: Terminates the process.
  case object Fpe extends Signal(8)
  // Kill.
  // Action: Terminates the process.
  case object Kill extends Signal(9)
  // User-defined signal 1.
  // Action: Terminates the process.
  case object Usr1 extends Signal(10)
  // Invalid memory reference.
  // Action: Terminates the process.
  case object Segv extends Signal(11)
  // User-defined signal 2.
  // Action: Terminates the process.
  case object Usr2 extends Signal(12)
  // Write on a pipe with no one to read it.
  // Action: Ignored.
  case object Pipe extends Signal(13)
  // Alarm clock.
  // Action: Terminates the process.
  case object Alrm extends Signal(14)
  // Termination signal.
  // Action: Terminates the process.
  case object Term extends Signal(15)
  // Child process terminated, stopped, or continued.
  // Action: Ignored.
  case object Chld extends Signal(16)
  // Continue executing, if stopped.
  // Action: Continues executing, if stopped.
  case object Cont extends Signal(17)
  // Stop executing.
  // Action: Stops executing.
  case object Stop extends Signal(18)
  // Terminal stop signal.
  // Action: Stops executing.
  case object Tstp extends Signal(19)
  // Background process attempting read.
  // Action: Stops executing.
  case object Ttin extends Signal(20)
  // Background process attempting write.
  // Action: Stops executing.
  case object Ttou extends Signal(21)
  // High bandwidth data is available at a socket.
  // Action: Ignored.
  case object Urg extends Signal(22)
  // CPU time limit exceeded.
  // Action: Terminates the process.
  case object Xcpu extends Signal(23)
  // File size limit exceeded.
  // Action: Terminates the process.
  case object Xfsz extends Signal(24)
  // Virtual timer expired.
  // Action: Terminates the process.
  case object Vtalrm extends Signal(25)
  // Profiling timer expired.
  // Action: Terminates the process.
  case object Prof extends Signal(26)
  // Window changed.
  // Action: Ignored.
  case object Winch extends Signal(27)
  // I/O possible.
  // Action: Terminates the process.
  case object Poll extends Signal(28)
  // Power failure.
  // Action: Terminates the process.
  case object Pwr extends Signal(29)
  // Bad system call.
  // Action: Terminates the process.
  case object Sys extends Signal(30)

  def values: IndexedSeq[Signal] = findValues

  implicit def reader[F[_]](implicit F: MonadError[F, Throwable]): SimpleValueReader[F, Signal] =
    new SimpleValueReader[F, Signal] {
      def read(v: Value): F[Signal] =
        v match {
          case Value.Int32(i) => withValueOpt(i.toByte).liftTo[F](new ConversionException(s"unknown signal $i"))
          case _              => F.raiseError(new ConversionException(s"expected i32 but got ${v.tpe}"))
        }
      val swamType: ValType = ValType.I32
    }
}

object Riflags {
  val RecvPeek: Riflags = 0x0000
  val RecvWaitall: Riflags = 0x0001
}

object Roflags {
  val RecvDataTruncated: Roflags = 0x0000
}

object Sdflags {
  val Rd: Sdflags = 0x00
  val Wr: Sdflags = 0x01
}
