package swam
package wasi
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}

import cats.effect.IO
import swam.runtime.Memory

object Types {
  // This is an autogenerated file, do not change it

  object subclockflagsFlags extends Enumeration {
    val subscription_clock_abstime = Value(0)
  }

  type ciovec_array = List[ciovec]
  type exitcode = u32

  case class `prestat`(mem: Memory[IO], offset: Int) extends WASI_STRUCT { // UNION
    val `dir` = prestat_dir(mem, offset + 0)

    def write(offset: Int, mem: Memory[IO]) = {
      dir.write(offset + 0, mem)
    }
  }

  type inode = u64
  type fd = Int

  object adviceEnum extends Enumeration {

    val `normal` = Value

    val `sequential` = Value

    val `random` = Value

    val `willneed` = Value

    val `dontneed` = Value

    val `noreuse` = Value
  }

  type s64 = Long
  type u8 = Byte

  case class `fdstat`(`fs_filetype`: Byte,
                      `fs_flags`: Short,
                      `fs_rights_base`: Long,
                      `fs_rights_inheriting`: Long /* extra fields */,
                      path: String = "",
                      fd: Int = -1)
      extends WASI_STRUCT {

    var position = 0

    def isDir() = (`fs_filetype` & filetypeEnum.`directory`.id) == filetypeEnum.`directory`.id
    def isRegularFile() = (`fs_filetype` & filetypeEnum.`regular_file`.id) == filetypeEnum.`regular_file`.id
    def isCharacterDevice() = (`fs_filetype` & filetypeEnum.`character_device`.id) == filetypeEnum.`character_device`.id
    def isBlockDevice() = (`fs_filetype` & filetypeEnum.`block_device`.id) == filetypeEnum.`block_device`.id
    def isSym() = (`fs_filetype` & filetypeEnum.`symbolic_link`.id) == filetypeEnum.`symbolic_link`.id
    def isSocket() = (`fs_filetype` & filetypeEnum.`socket_stream`.id) == filetypeEnum.`socket_stream`.id

    def write(bytes: Array[Byte]): Int = {
      if (fd == 0) // stdin
        throw new WASIException(errnoEnum.`perm`)

      if (fd == 1) // stdout
        {
          System.out.write(bytes, 0, bytes.length)
          return bytes.length
        }
      if (fd == 0) // stderr
        {
          System.err.write(bytes, 0, bytes.length)
          return bytes.length
        }

      val file = Paths.get(path).toFile

      if (!file.canWrite)
        throw new WASIException(errnoEnum.`perm`)

      val writer = Files.newBufferedWriter(Paths.get(path), StandardOpenOption.WRITE)

      // TODO check for encoding
      writer.write(bytes.map(_.toChar), 0, bytes.length)

      writer.close()
      bytes.length
    }

    def read(offset: Int, bytes: Array[Byte], len: Int): Int = {

      if (fd == 1) // stdout
        throw new WASIException(errnoEnum.`perm`)

      if (fd == 2) // stderr
        throw new WASIException(errnoEnum.`perm`)

      if (fd == 0) // stdin
        {
          System.in.read(bytes, 0, len)
          return len
        }
      // Then is a file

      // Check protocol

      val file = Paths.get(path).toFile

      if (!file.canRead)
        throw new WASIException(errnoEnum.`perm`)

      if (position == file.length())
        return 0

      val reader = Files.newBufferedReader(Paths.get(path), Charset.forName("UTF-8"))
      reader.skip(position)

      val chars = new Array[Char](len)
      val read = reader.read(chars, 0, len)

      if (read > 0) {
        chars.map(_.toByte).copyToArray(bytes)
        position += read
      }
      reader.close()
      Math.max(0, read)
    }

    def truncate(size: Long) = {
      val p = Paths.get(path)
      val fc = FileChannel.open(p, StandardOpenOption.WRITE)
      fc.truncate(size)
    }

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeByte(offset + 0, `fs_filetype`).unsafeRunSync

      mem.writeShort(offset + 2, `fs_flags`).unsafeRunSync
      mem.writeShort(offset + 4, 0).unsafeRunSync

      mem.writeLong(offset + 8, `fs_rights_base`).unsafeRunSync

      mem.writeLong(offset + 16, `fs_rights_inheriting`).unsafeRunSync

    }
  }

  case class `ciovec`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `buf` = new Pointer[u8](mem.readInt(offset).unsafeRunSync,
                                (i) => mem.readByte(i).unsafeRunSync(),
                                (i, r) => mem.writeByte(i, `r`).unsafeRunSync)
    val `buf_len` = mem.readInt(offset + 4).unsafeRunSync

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeInt(offset + 0, `buf`.offset).unsafeRunSync

      mem.writeInt(offset + 4, `buf_len`).unsafeRunSync

    }
  }

  object signalEnum extends Enumeration {

    val `none` = Value

    val `hup` = Value

    val `int` = Value

    val `quit` = Value

    val `ill` = Value

    val `trap` = Value

    val `abrt` = Value

    val `bus` = Value

    val `fpe` = Value

    val `kill` = Value

    val `usr1` = Value

    val `segv` = Value

    val `usr2` = Value

    val `pipe` = Value

    val `alrm` = Value

    val `term` = Value

    val `chld` = Value

    val `cont` = Value

    val `stop` = Value

    val `tstp` = Value

    val `ttin` = Value

    val `ttou` = Value

    val `urg` = Value

    val `xcpu` = Value

    val `xfsz` = Value

    val `vtalrm` = Value

    val `prof` = Value

    val `winch` = Value

    val `poll` = Value

    val `pwr` = Value

    val `sys` = Value
  }

  type dirnamlen = u32
  type userdata = u64

  case class `subscription_clock`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `id` = mem.readInt(offset + 0).unsafeRunSync

    val `timeout` = mem.readLong(offset + 4).unsafeRunSync

    val `precision` = mem.readLong(offset + 12).unsafeRunSync

    val `flags` = mem.readShort(offset + 20).unsafeRunSync

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeInt(offset + 0, `id`).unsafeRunSync

      mem.writeLong(offset + 4, `timeout`).unsafeRunSync

      mem.writeLong(offset + 12, `precision`).unsafeRunSync

      mem.writeShort(offset + 20, `flags`).unsafeRunSync

    }
  }

  object eventtypeEnum extends Enumeration {

    val `clock` = Value

    val `fd_read` = Value(0x0000000000000002)

    val `fd_write` = Value(0x0000000000000040)
  }

  type filesize = u64
  type u32 = Int
  type filedelta = s64

  object errnoEnum extends Enumeration {

    val `success` = Value(0)

    val `2big` = Value(1)

    val `acces` = Value(2)

    val `addrinuse` = Value(3)

    val `addrnotavail` = Value(4)

    val `afnosupport` = Value(5)

    val `again` = Value(6)

    val `already` = Value(7)

    val `badf` = Value(8)

    val `badmsg` = Value(9)

    val `busy` = Value(10)

    val `canceled` = Value(11)

    val `child` = Value(12)

    val `connaborted` = Value(13)

    val `connrefused` = Value(14)

    val `connreset` = Value(15)

    val `deadlk` = Value(16)

    val `destaddrreq` = Value(17)

    val `dom` = Value(18)

    val `dquot` = Value(19)

    val `exist` = Value(20)

    val `fault` = Value(21)

    val `fbig` = Value(22)

    val `hostunreach` = Value(23)

    val `idrm` = Value(24)

    val `ilseq` = Value(25)

    val `inprogress` = Value(26)

    val `intr` = Value(27)

    val `inval` = Value(28)

    val `io` = Value(29)

    val `isconn` = Value(30)

    val `isdir` = Value(31)

    val `loop` = Value(32)

    val `mfile` = Value(33)

    val `mlink` = Value(34)

    val `msgsize` = Value(35)

    val `multihop` = Value(36)

    val `nametoolong` = Value(37)

    val `netdown` = Value(38)

    val `netreset` = Value(39)

    val `netunreach` = Value(40)

    val `nfile` = Value(41)

    val `nobufs` = Value(42)

    val `nodev` = Value(43)

    val `noent` = Value(44)

    val `noexec` = Value(45)

    val `nolck` = Value(46)

    val `nolink` = Value(47)

    val `nomem` = Value(48)

    val `nomsg` = Value(49)

    val `noprotoopt` = Value(50)

    val `nospc` = Value(51)

    val `nosys` = Value(52)

    val `notconn` = Value(53)

    val `notdir` = Value(54)

    val `notempty` = Value(55)

    val `notrecoverable` = Value(56)

    val `notsock` = Value(57)

    val `notsup` = Value(58)

    val `notty` = Value(59)

    val `nxio` = Value(60)

    val `overflow` = Value(61)

    val `ownerdead` = Value(62)

    val `perm` = Value(63)

    val `pipe` = Value(64)

    val `proto` = Value(65)

    val `protonosupport` = Value(66)

    val `prototype` = Value(67)

    val `range` = Value(68)

    val `rofs` = Value(69)

    val `spipe` = Value(70)

    val `srch` = Value(71)

    val `stale` = Value(72)

    val `timedout` = Value(73)

    val `txtbsy` = Value(74)

    val `xdev` = Value(75)

    val `notcapable` = Value(76)
  }

  object lookupflagsFlags extends Enumeration {
    val symlink_follow = Value(0)
  }

  type timestamp = u64
  type size = u32

  case class `dirent`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `d_next` = mem.readLong(offset + 0).unsafeRunSync

    val `d_ino` = mem.readLong(offset + 8).unsafeRunSync

    val `d_namlen` = mem.readInt(offset + 16).unsafeRunSync

    val `d_type` = mem.readByte(offset + 20).unsafeRunSync()

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeLong(offset + 0, `d_next`).unsafeRunSync

      mem.writeLong(offset + 8, `d_ino`).unsafeRunSync

      mem.writeInt(offset + 16, `d_namlen`).unsafeRunSync

      mem.writeByte(offset + 20, `d_type`).unsafeRunSync

    }
  }

  case class `subscription_fd_readwrite`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `file_descriptor` = mem.readInt(offset + 0).unsafeRunSync

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeInt(offset + 0, `file_descriptor`).unsafeRunSync

    }
  }

  case class `event`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `userdata` = mem.readLong(offset + 0).unsafeRunSync

    val `error` = mem.readShort(offset + 8).unsafeRunSync

    val `type` = mem.readByte(offset + 10).unsafeRunSync()

    val `fd_readwrite` = event_fd_readwrite(mem, offset + 12)

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeLong(offset + 0, `userdata`).unsafeRunSync

      mem.writeShort(offset + 8, `error`).unsafeRunSync

      mem.writeByte(offset + 10, `type`).unsafeRunSync

      fd_readwrite.write(offset + 12, mem)
    }
  }

  case class `filestat`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `dev` = mem.readLong(offset + 0).unsafeRunSync

    val `ino` = mem.readLong(offset + 8).unsafeRunSync

    val `filetype` = mem.readByte(offset + 16).unsafeRunSync()

    val `nlink` = mem.readLong(offset + 18).unsafeRunSync

    val `size` = mem.readLong(offset + 26).unsafeRunSync

    val `atim` = mem.readLong(offset + 34).unsafeRunSync

    val `mtim` = mem.readLong(offset + 42).unsafeRunSync

    val `ctim` = mem.readLong(offset + 50).unsafeRunSync

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeLong(offset + 0, `dev`).unsafeRunSync

      mem.writeLong(offset + 8, `ino`).unsafeRunSync

      mem.writeByte(offset + 16, `filetype`).unsafeRunSync

      mem.writeLong(offset + 18, `nlink`).unsafeRunSync

      mem.writeLong(offset + 26, `size`).unsafeRunSync

      mem.writeLong(offset + 34, `atim`).unsafeRunSync

      mem.writeLong(offset + 42, `mtim`).unsafeRunSync

      mem.writeLong(offset + 50, `ctim`).unsafeRunSync

    }
  }

  object filetypeEnum extends Enumeration {

    val `unknown` = Value

    val `block_device` = Value

    val `character_device` = Value

    val `directory` = Value

    val `regular_file` = Value

    val `socket_dgram` = Value

    val `socket_stream` = Value

    val `symbolic_link` = Value
  }

  object sdflagsFlags extends Enumeration {
    val rd = Value(0)

    val wr = Value(1)
  }

  case class `subscription`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `userdata` = mem.readLong(offset + 0).unsafeRunSync

    val `u` = subscription_u(mem, offset + 8)

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeLong(offset + 0, `userdata`).unsafeRunSync

      u.write(offset + 8, mem)
    }
  }

  type string = String

  object fstflagsFlags extends Enumeration {
    val atim = Value(0)

    val atim_now = Value(1)

    val mtim = Value(2)

    val mtim_now = Value(3)
  }

  object roflagsFlags extends Enumeration {
    val recv_data_truncated = Value(0)
  }

  type dircookie = u64
  type iovec_array = List[iovec]

  object riflagsFlags extends Enumeration {
    val recv_peek = Value(0)

    val recv_waitall = Value(1)
  }

  case class `event_fd_readwrite`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `nbytes` = mem.readLong(offset + 0).unsafeRunSync

    val `flags` = mem.readShort(offset + 8).unsafeRunSync

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeLong(offset + 0, `nbytes`).unsafeRunSync

      mem.writeShort(offset + 8, `flags`).unsafeRunSync

    }
  }

  type u64 = Long

  case class `subscription_u`(mem: Memory[IO], offset: Int) extends WASI_STRUCT { // UNION
    val `clock` = subscription_clock(mem, offset + 0)
    val `fd_read` = subscription_fd_readwrite(mem, offset + 22)
    val `fd_write` = subscription_fd_readwrite(mem, offset + 26)

    def write(offset: Int, mem: Memory[IO]) = {
      clock.write(offset + 0, mem)
      fd_read.write(offset + 22, mem)
      fd_write.write(offset + 26, mem)
    }
  }

  object clockidEnum extends Enumeration {

    val `realtime` = Value

    val `monotonic` = Value

    val `process_cputime_id` = Value

    val `thread_cputime_id` = Value
  }

  case class `iovec`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {

    val `buf` = new Pointer[u8](mem.readInt(offset).unsafeRunSync,
                                (i) => mem.readByte(i).unsafeRunSync(),
                                (i, r) => mem.writeByte(i, `r`).unsafeRunSync)
    val `buf_len` = mem.readInt(offset + 4).unsafeRunSync

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeInt(offset + 0, `buf`.offset).unsafeRunSync

      mem.writeInt(offset + 4, `buf_len`).unsafeRunSync

    }
  }

  object oflagsFlags extends Enumeration {
    val creat = Value(0)

    val directory = Value(1)

    val excl = Value(2)

    val trunc = Value(3)
  }

  object whenceEnum extends Enumeration {

    val `set` = Value

    val `cur` = Value

    val `end` = Value
  }

  type siflags = u16
  type ptr = Int
  type linkcount = u64

  object rightsFlags extends Enumeration {
    val fd_datasync = Value(0x0000000000000001)

    val fd_read = Value(0x0000000000000002)

    val fd_seek = Value(0x0000000000000004)

    val fd_fdstat_set_flags = Value(0x0000000000000008)

    val fd_sync = Value(0x0000000000000010)

    val fd_tell = Value(0x0000000000000020)

    val fd_write = Value(0x0000000000000040)

    val fd_advise = Value(0x0000000000000080)

    val fd_allocate = Value(0x0000000000000100)

    val path_create_directory = Value(0x0000000000000200)

    val path_create_file = Value(0x0000000000000400)

    val path_link_source = Value(0x0000000000000800)

    val path_link_target = Value(0x0000000000001000)

    val path_open = Value(0x0000000000002000)

    val fd_readdir = Value(0x0000000000004000)

    val path_readlink = Value(0x0000000000008000)

    val path_rename_source = Value(0x0000000000010000)

    val path_rename_target = Value(0x0000000000020000)

    val path_filestat_get = Value(0x0000000000040000)

    val path_filestat_set_size = Value(0x0000000000080000)

    val path_filestat_set_times = Value(0x0000000000100000)

    val fd_filestat_get = Value(0x0000000000200000)

    val fd_filestat_set_size = Value(0x0000000000400000)

    val fd_filestat_set_times = Value(0x0000000000800000)

    val path_symlink = Value(0x0000000001000000)

    val path_remove_directory = Value(0x0000000002000000)

    val path_unlink_file = Value(0x0000000004000000)

    val poll_fd_readwrite = Value(0x0000000008000000)

    val sock_shutdown = Value(0x0000000010000000)
  }

  object preopentypeEnum extends Enumeration {

    val `dir` = Value
  }

  object eventrwflagsFlags extends Enumeration {
    val fd_readwrite_hangup = Value(0)
  }

  object fdflagsFlags extends Enumeration {
    val append = Value(0)

    val dsync = Value(1)

    val nonblock = Value(2)

    val rsync = Value(3)

    val sync = Value(4)
  }

  type device = u64
  type u16 = Short

  case class `prestat_dir`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
    val `pr_name_len` = mem.readInt(offset + 0).unsafeRunSync

    def write(offset: Int, mem: Memory[IO]) = {
      mem.writeInt(offset + 0, `pr_name_len`).unsafeRunSync

    }
  }

}
