package swam
package wasi

import java.io.{File, FileDescriptor, FileInputStream, FileOutputStream, PrintWriter}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.attribute.{BasicFileAttributeView, FileAttribute, FileTime, PosixFilePermissions}
import java.nio.file.{Files, NoSuchFileException, Path, Paths}

import cats.effect.IO
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._
import swam.runtime.imports.annotations.{effect, module}
import swam.runtime.{Memory, pageSize}
import swam.wasi.Types._
import swam.wasi.TypesHeader._
import cats.Applicative

import scala.collection.immutable.HashMap

/**
    @author Javier Cabrera-Arteaga on 2020-03-23
    Follow NodeJS implementation https://github.com/nodejs/wasi/tree/wasi/lib of WASI to implement this
  */
class WASIImplementation[@effect F[_]](val args: Seq[String], val dirs: Vector[String])(implicit F: Applicative[F])
    extends Module[F] {

  var fdMap: Map[Int, `fdstat`] = HashMap[Int, `fdstat`](
    0 -> `fdstat`(filetypeEnum.`character_device`.id.toByte, 0, STDIN_DEFAULT_RIGHTS, 0, fd = 0),
    1 -> `fdstat`(filetypeEnum.`character_device`.id.toByte, 0, STDOUT_DEFAULT_RIGHTS, 0, fd = 1),
    2 -> `fdstat`(filetypeEnum.`character_device`.id.toByte, 0, STDERR_DEFAULT_RIGHTS, 0, fd = 2)
  ) ++ (3 until (3 + dirs.length)) // preopened fd
    .map(
      i =>
        (i,
         `fdstat`(filetypeEnum.`directory`.id.toByte,
                  0,
                  RIGHTS_DIRECTORY_BASE,
                  RIGHTS_DIRECTORY_INHERITING,
                  dirs(i - 3),
                  fd = i)))
    .toMap

  /**
    * Returns stat numbers: filetype rightsBase rightsInheriting
    *
    * @param fd
    */
  def parseStats(fd: Int): `fdstat` = {

    if (!fdMap.contains(fd) || fdMap(fd) == null)
      throw new WASIException(errnoEnum.`badf`)

    val entry = fdMap(fd)
    // Update entry

    entry
  }

  def getModule() = asInstanceOf[Module[F]]

  def checkRight(fd: fd, rights: Int): `fdstat` = {
    val stat = parseStats(fd)

    if (rights != 0 && (stat.`fs_rights_base` & rights) == 0)
      throw new WASIException(errno = errnoEnum.`perm`) // raise errno exception

    stat
  }

  val CPU_START = System.nanoTime()

  def now(id: clockidEnum.Value) = {
    id match {
      case clockidEnum.`monotonic`          => System.nanoTime()
      case clockidEnum.`realtime`           => System.nanoTime()
      case clockidEnum.`process_cputime_id` => System.nanoTime() - CPU_START
      case clockidEnum.`thread_cputime_id`  => System.nanoTime() - CPU_START
    }

    -1
  }
  override def args_getImpl(argv: ptr, argv_buf: ptr): Types.errnoEnum.Value = {
    var coffset = argv
    var offset = argv_buf

    args.foreach(arg => {
      mem.writeInt(coffset, offset).unsafeRunSync()
      coffset += 4
      mem.writeBytes(offset, ByteBuffer.wrap(arg.getBytes())).unsafeRunSync()
      offset += arg.getBytes().length + 1
    })
    errnoEnum.`success`
  }

  override def args_sizes_getImpl(argc: ptr, argv_buf_size: ptr): Types.errnoEnum.Value = {
    mem.writeInt(argc, args.length).unsafeRunSync()
    mem.writeInt(argv_buf_size, args.map(t => t.length + 1).sum).unsafeRunSync()
    errnoEnum.`success`
  }

  override def fd_pwriteImpl(fd: fd,
                             iovs: ciovec_array,
                             iovsLen: u32,
                             offset: filesize,
                             nwritten: ptr): errnoEnum.Value = errnoEnum.`noent`

  override def environ_getImpl(environ: ptr, environ_buf: ptr): Types.errnoEnum.Value = {
    var coffset = environ
    var offset = environ_buf

    System.getenv().forEach {
      case (key, value) => {
        mem.writeInt(coffset, offset).unsafeRunSync()
        coffset += 4
        val bytes = new Array[Byte](s"$key=$value".getBytes().length + 1)
        //mem.position(offset)
        mem.writeBytes(offset, ByteBuffer.wrap(s"$key=$value".getBytes())).unsafeRunSync()
        //mem.position(oldPos)
        offset += bytes.length
      }
    }
    errnoEnum.`success`

  }

  override def environ_sizes_getImpl(environc: ptr, environ_buf_size: ptr): Types.errnoEnum.Value = {
    var cumul = 0
    System.getenv().forEach {
      case (key, value) => {
        val processed = s"$key=$value"
        cumul += processed.getBytes().length + 1
      }
    }

    mem.writeInt(environc, System.getenv().size()).unsafeRunSync()
    mem.writeInt(environ_buf_size, cumul).unsafeRunSync()
    errnoEnum.`success`
  }

  override def clock_res_getImpl(id: Types.clockidEnum.Value, resolution: ptr): Types.errnoEnum.Value = {
    mem.writeLong(resolution, 0L)
    errnoEnum.`success`
  }

  override def clock_time_getImpl(id: Types.clockidEnum.Value,
                                  precision: timestamp,
                                  time: ptr): Types.errnoEnum.Value = {
    val nowVal = now(id)

    if (nowVal < 0)
      throw new WASIException(errnoEnum.`inval`)

    mem.writeLong(time, nowVal)
    errnoEnum.`success`
  }

  override def fd_adviseImpl(fd: fd,
                             offset: filesize,
                             len: filesize,
                             advice: Types.adviceEnum.Value): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_advise.id)
    errnoEnum.`nosys`
  }

  override def fd_allocateImpl(fd: fd, offset: filesize, len: filesize): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_allocate.id)
    errnoEnum.`nosys`
  }

  override def fd_closeImpl(fd: fd): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_datasync.id)
    if (fdMap.contains(fd))
      throw new WASIException(errnoEnum.`badf`)

    fdMap = fdMap.updated(fd, null) // delete the entry

    errnoEnum.`success`
  }

  override def fd_datasyncImpl(fd: fd): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_datasync.id)
    // TODO
    errnoEnum.`success`
  }

  override def fd_fdstat_getImpl(fd: fd, stat: ptr): Types.errnoEnum.Value = {

    val st = checkRight(fd, 0)
    st.write(stat, mem)
    Types.errnoEnum.`success`
  }

  override def fd_fdstat_set_flagsImpl(fd: fd, flags: Types.fdflagsFlags.Value): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_fdstat_set_flags.id)
    errnoEnum.`nosys`
  }

  override def fd_fdstat_set_rightsImpl(fd: fd,
                                        fs_rights_base: Types.rightsFlags.Value,
                                        fs_rights_inheriting: Types.rightsFlags.Value): Types.errnoEnum.Value = {

    var stat = checkRight(fd, 0)
    val nrb = stat.`fs_rights_base` | fs_rights_base.id

    if (nrb > stat.`fs_rights_base`)
      errnoEnum.`perm`

    val nri = stat.`fs_rights_inheriting` | fs_rights_inheriting.id

    if (nri > stat.`fs_rights_inheriting`)
      errnoEnum.`perm`

    stat = stat.copy(`fs_rights_inheriting` = nrb)
    stat = stat.copy(`fs_rights_base` = nri)

    fdMap = fdMap.updated(fd, stat)
    errnoEnum.`success`
  }

  override def fd_filestat_getImpl(fd: fd, buf: ptr): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_filestat_get.id)
    // Since we are using the real fd we dont need to get it from fs
    stat.write(buf, mem)
    errnoEnum.`success`
  }

  override def fd_filestat_set_sizeImpl(fd: fd, size: filesize): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_filestat_set_size.id)
    stat.truncate(size)
    errnoEnum.`success`
  }

  override def fd_filestat_set_timesImpl(fd: fd,
                                         atim: timestamp,
                                         mtim: timestamp,
                                         fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_filestat_set_times.id)
    val n = now(clockidEnum.`realtime`)

    val atimNow = (fst_flags.id & WASI_FILESTAT_SET_ATIM_NOW) == WASI_FILESTAT_SET_ATIM_NOW
    val mtimNow = (fst_flags.id & WASI_FILESTAT_SET_MTIM_NOW) == WASI_FILESTAT_SET_MTIM_NOW

    val atime: Long = if (atimNow) n else atim
    val mtime: Long = if (mtimNow) n else mtim

    stat.changeTimestamps(atime, mtime)
    errnoEnum.`success`
  }

  override def fd_preadImpl(fd: fd,
                            iovs: iovec_array,
                            iovsLen: u32,
                            offset: filesize,
                            nread: ptr): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.fd_read.id | rightsFlags.fd_seek.id)

    val cumul = iovs
      .map(iov => {
        val dst = new Array[Byte](iov.`buf_len`)
        val read = stat.read(0, dst, iov.`buf_len`, updatePosition = false) // seek
        if (read > 0)
          mem.writeBytes(iov.buf.offset, ByteBuffer.wrap(dst, 0, read)).unsafeRunSync()
        read
      })
      .sum

    //nread._set(0, 0)
    mem.writeInt(nread, cumul).unsafeRunSync()
    Types.errnoEnum.`success`
  }

  override def fd_prestat_getImpl(fd: fd, buf: ptr): Types.errnoEnum.Value = {

    val stats = checkRight(fd, 0)

    mem.writeByte(buf, WASI_PREOPENTYPE_DIR.toByte).unsafeRunSync()
    mem.writeInt(buf + 4, stats.path.getBytes.length).unsafeRunSync()

    errnoEnum.`success`
  }

  override def fd_prestat_dir_nameImpl(fd: fd, path: ptr, path_len: size): Types.errnoEnum.Value = {
    val stats = checkRight(fd, 0)

    mem.writeBytes(path, ByteBuffer.wrap(stats.path.getBytes)).unsafeRunSync()

    errnoEnum.`success`
  }

  override def fd_readImpl(fd: fd, iovs: iovec_array, iovsLen: u32, nread: ptr): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_read.id)

    val cumul = iovs
      .map(iov => {
        val dst = new Array[Byte](iov.`buf_len`)
        val read = stat.read(0, dst, iov.`buf_len`)
        if (read > 0)
          mem.writeBytes(iov.buf.offset, ByteBuffer.wrap(dst, 0, read)).unsafeRunSync()
        read
      })
      .sum

    mem.writeInt(nread, cumul).unsafeRunSync()
    Types.errnoEnum.`success`
  }

  def getFileType(entry: Path) = {
    if (entry.toFile.isDirectory)
      filetypeEnum.`directory`
    if (entry.toFile.isFile)
      filetypeEnum.`regular_file`
    // TODO add other cases
    filetypeEnum.`unknown`
  }

  override def fd_readdirImpl(fd: fd,
                              buf: ptr,
                              buf_len: size,
                              cookie: dircookie,
                              bufused: ptr): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_readdir.id)

    val entries: Array[Path] = stat.readdir()

    var bufPtr = buf
    for (i <- cookie until entries.length) {
      val entry = entries(i.toInt)
      mem.writeLong(bufPtr, i + 1).unsafeRunSync()
      bufPtr += 8

      if (bufPtr - buf < buf_len) {
        val resolve = Paths.get(stat.path).resolve(entry)
        val name = resolve.getFileName.toString.getBytes
        // TODO write ino
        bufPtr += 8

        if (bufPtr - buf < buf_len) {
          mem.writeInt(bufPtr, name.length).unsafeRunSync()
          bufPtr += 4
          if (bufPtr - buf < buf_len) {
            val fileType: filetypeEnum.Value = getFileType(entry)

            mem.writeByte(bufPtr, fileType.id.toByte).unsafeRunSync

            bufPtr += 1

            bufPtr += 3 // padding

            if (bufPtr - buf < buf_len) {
              mem.writeBytes(bufPtr, ByteBuffer.wrap(name)).unsafeRunSync
              bufPtr += name.length
            }
          }
        }
      }
    }
    mem.writeInt(bufused, Math.min(bufPtr - buf, buf_len)).unsafeRunSync
    Types.errnoEnum.`success`
  }

  override def fd_renumberImpl(fd: fd, to: fd): Types.errnoEnum.Value = {
    val from = checkRight(fd, 0)
    val toStat = checkRight(to, 0)

    // TODO check close
    fdMap = fdMap.updated(fd, fdMap(to))
    fdMap = fdMap.updated(to, null)

    Types.errnoEnum.`success`
  }

  override def fd_seekImpl(fd: fd,
                           offset: filedelta,
                           whence: Types.whenceEnum.Value,
                           newoffset: ptr): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_seek.id)

    val newPos: Long = whence match {
      case whenceEnum.`cur` => stat.position // TODO check this case
      case whenceEnum.`end` => Paths.get(stat.path).toFile.length + stat.position
      case whenceEnum.`set` => offset
    }

    stat.position = newPos.toInt
    mem.writeLong(newoffset, newPos).unsafeRunSync()
    Types.errnoEnum.`success`
  }

  override def fd_syncImpl(fd: fd): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_sync.id)

    // TODO check if we need to do so
    Types.errnoEnum.`success`
  }

  override def fd_tellImpl(fd: fd, offset: ptr): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.fd_tell.id)

    mem.writeLong(offset, stat.position).unsafeRunSync
    Types.errnoEnum.`success`
  }

  override def fd_writeImpl(fd: fd, iovs: ciovec_array, iovsLen: u32, nwritten: Int): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.fd_write.id)

    val cumul =
      iovs
        .map(iov => {
          val dst = new Array[Byte](iov.`buf_len`)
          mem.readBytes(iov.`buf`.offset, dst).unsafeRunSync()
          stat.write(dst)
          iov.`buf_len`
        })
        .sum
    mem.writeInt(nwritten, cumul).unsafeRunSync()

    Types.errnoEnum.`success`
  }

  override def path_create_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.path_create_directory.id)

    if (stat.path.isEmpty)
      throw new WASIException(errnoEnum.`inval`)

    val p = Paths.get(path)
    Files.createDirectory(p)

    Types.errnoEnum.`success`
  }

  override def path_filestat_getImpl(fd: fd,
                                     flags: Types.lookupflagsFlags.Value,
                                     path: string,
                                     buf: ptr): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.fd_filestat_get.id)

    if (stat.path.isEmpty)
      throw new WASIException(errnoEnum.`inval`)

    val resolved = Paths.get(stat.path).resolve(path)

    try resolved.toRealPath()
    catch {
      case _: NoSuchFileException => throw new WASIException(errnoEnum.`inval`)
    }

    // mem.writeLong() // write dev
    // mem.writeLong() // write dev
    // buf + 16
    // TODO posix format data

    mem.writeByte(buf + 16, getFileType(resolved).id.toByte).unsafeRunSync

    Types.errnoEnum.`success`

  }

  override def path_filestat_set_timesImpl(fd: fd,
                                           flags: Types.lookupflagsFlags.Value,
                                           path: string,
                                           atim: timestamp,
                                           mtim: timestamp,
                                           fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.path_filestat_set_times.id)
    val n = now(clockidEnum.`realtime`)

    val atimNow = (fst_flags.id & WASI_FILESTAT_SET_ATIM_NOW) == WASI_FILESTAT_SET_ATIM_NOW
    val mtimNow = (fst_flags.id & WASI_FILESTAT_SET_MTIM_NOW) == WASI_FILESTAT_SET_MTIM_NOW

    val atime: Long = if (atimNow) n else atim
    val mtime: Long = if (mtimNow) n else mtim

    val p = Paths.get(stat.path).resolve(path).toRealPath()

    Files
      .getFileAttributeView(p, classOf[BasicFileAttributeView])
      .setTimes(FileTime.fromMillis(mtime), FileTime.fromMillis(atime), null)

    Types.errnoEnum.`success`
  }

  override def path_linkImpl(old_fd: fd,
                             old_flags: Types.lookupflagsFlags.Value,
                             old_path: string,
                             new_fd: fd,
                             new_path: string): Types.errnoEnum.Value = {

    val ostat = checkRight(old_fd, rightsFlags.path_link_source.id)
    val nstat = checkRight(new_fd, rightsFlags.path_link_target.id)

    if (ostat.path.isEmpty || nstat.path.isEmpty)
      throw new WASIException(errnoEnum.`inval`)

    Files.createSymbolicLink(Paths.get(ostat.path).resolve(old_path), Paths.get(nstat.path).resolve(new_path))
    Types.errnoEnum.`success`
  }

  override def path_readlinkImpl(fd: fd, path: string, buf: ptr, buf_len: size, bufused: ptr): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.path_readlink.id)

    val full = Paths.get(stat.path).resolve(path)
    val r = Files.readSymbolicLink(full)
    val dst = new Array[Char](buf_len)

    val reader = Files.newBufferedReader(r, Charset.forName("UTF-8"))
    val newL = reader.read(dst, 0, dst.length)

    mem.writeBytes(buf, ByteBuffer.wrap(dst.map(_.toByte), 0, newL))
    mem.writeInt(bufused, Math.min(newL, newL))
    Types.errnoEnum.`success`
  }

  override def path_remove_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.path_remove_directory.id)

    if (stat.path.isEmpty)
      throw new WASIException(errnoEnum.`inval`)

    Files.delete(Paths.get(stat.path).resolve(path).toRealPath())
    Types.errnoEnum.`success`
  }

  override def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): Types.errnoEnum.Value = {

    val ostats = checkRight(fd, rightsFlags.path_rename_source.id)
    val nstats = checkRight(new_fd, rightsFlags.path_rename_target.id)

    val oPath = Paths.get(ostats.path).resolve(old_path).toRealPath()
    val nPath = Paths.get(nstats.path).resolve(new_path).toRealPath()

    Files.move(oPath, nPath)
    Types.errnoEnum.`success`
  }

  override def path_symlinkImpl(old_path: string, fd: fd, new_path: string): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.path_symlink.id)

    if (stat.path.isEmpty)
      throw new WASIException(errnoEnum.`inval`)

    val oPath = Paths.get(stat.path).resolve(old_path).toRealPath()
    val nPath = Paths.get(stat.path).resolve(new_path).toRealPath()

    Files.createSymbolicLink(oPath, nPath)
    Types.errnoEnum.`success`
  }

  override def path_unlink_fileImpl(fd: fd, path: string): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.path_unlink_file.id)

    if (stat.path.isEmpty)
      throw new WASIException(errnoEnum.`inval`)

    val oPath = Paths.get(stat.path).resolve(path).toRealPath()

    Files.delete(oPath)
    Types.errnoEnum.`success`
  }

  override def poll_oneoffImpl(in: ptr, out: ptr, nsubscriptions: size, nevents: ptr): Types.errnoEnum.Value = {

    // TODO
    errnoEnum.`fault`
  }

  override def proc_exitImpl(rval: exitcode): Unit = {
    System.exit(rval)
  }

  override def proc_raiseImpl(sig: Types.signalEnum.Value): Types.errnoEnum.Value = {
    if (!(sig.id >= 0 && sig.id <= 24))
      errnoEnum.`inval`

    System.exit(sig.id)
    errnoEnum.`success`
  }

  override def sched_yieldImpl(): Types.errnoEnum.Value = {
    errnoEnum.`success`
  }

  override def random_getImpl(buf: ptr, buf_len: size): Types.errnoEnum.Value = {
    val random = Range(0, buf_len).map(_ => (255 * Math.random()).toByte).toArray
    mem.writeBytes(buf, ByteBuffer.wrap(random)).unsafeRunSync
    errnoEnum.`success`
  }

  override def sock_recvImpl(fd: fd,
                             ri_data: iovec_array,
                             ri_dataLen: u32,
                             ri_flags: Types.riflagsFlags.Value,
                             ro_datalen: ptr,
                             ro_flags: ptr): Types.errnoEnum.Value = {
    errnoEnum.`nosys`
  }

  override def sock_sendImpl(fd: fd,
                             si_data: ciovec_array,
                             si_dataLen: u32,
                             si_flags: siflags,
                             so_datalen: ptr): Types.errnoEnum.Value = {
    errnoEnum.`nosys`
  }

  override def sock_shutdownImpl(fd: fd, how: Types.sdflagsFlags.Value): Types.errnoEnum.Value = {
    errnoEnum.`nosys`
  }

  override def path_openImpl(fd: fd,
                             dirflags: Int,
                             path: ptr,
                             pathLen: Int,
                             oflags: Int,
                             fs_rights_base: Long,
                             fs_rights_inherting: Long,
                             fdflags: Int,
                             opened_fd: ptr): Types.errnoEnum.Value = {
    val stat = checkRight(fd, rightsFlags.path_open.id)

    val read = (fs_rights_base & (rightsFlags.fd_read.id | rightsFlags.fd_readdir.id)) != 0
    val write = (fs_rights_base & (rightsFlags.fd_datasync.id | rightsFlags.fd_write.id | rightsFlags.fd_allocate.id | rightsFlags.path_filestat_set_size.id)) != 0

    var noflags = {
      if (write && read)
        O_RDWR
      else if (read)
        O_RDONLY
      else
        O_WRONLY
    }

    var neededBase = fs_rights_base | rightsFlags.path_open.id
    var neededInheriting = fs_rights_base | fs_rights_inherting

    if ((oflags & WASI_O_CREAT) != 0) {
      noflags |= O_CREAT
      neededBase |= rightsFlags.path_create_file.id
    }

    if ((oflags & WASI_O_DIRECTORY) != 0) {
      noflags |= O_DIRECTORY
    }

    if ((oflags & WASI_O_EXCL) != 0) {
      noflags |= O_EXCL
    }

    if ((oflags & WASI_O_TRUNC) != 0) {
      noflags |= O_TRUNC
      neededBase |= rightsFlags.path_filestat_set_size.id
    }

    if ((fdflags & WASI_FD_FLAG_APPEND) != 0) {
      noflags |= O_APPEND
    }

    if ((fdflags & WASI_FD_FLAG_DSYNC) != 0) {
      // TODO check
      noflags |= O_SYNC
      neededInheriting |= rightsFlags.fd_datasync.id
    }

    if ((fdflags & WASI_FD_RSYNC) != 0) {
      noflags |= O_SYNC
      neededInheriting |= rightsFlags.fd_sync.id
    }

    if ((fdflags & WASI_FD_SYNC) != 0) {
      noflags |= O_SYNC
      neededInheriting |= rightsFlags.fd_sync.id
    }

    if (write && ((noflags & (O_APPEND | O_TRUNC)) == 0)) {
      neededInheriting |= rightsFlags.fd_seek.id
    }

    val dst = new Array[Byte](pathLen)
    mem.readBytes(path, dst).unsafeRunSync()
    val str = new String(dst)

    val relative = Paths.get(stat.path).resolve(str)

    if (relative.startsWith("..")) {
      throw new WASIException(errnoEnum.`notcapable`)
    }

    // TODO create posix file permissions

    val absolute =
      try {
        relative.toRealPath()
      } catch {
        case _: NoSuchFileException =>
          if ((noflags & O_CREAT) == O_CREAT)
            Files.createFile(relative).toRealPath()
          else
            throw new WASIException(errnoEnum.`badf`)
      }

    val newFd = fdMap.size
    fdMap = fdMap
      .updated(newFd,
               `fdstat`(filetypeEnum.`unknown`.id.toByte, 0, neededBase, neededInheriting, absolute.toString, newFd))

    mem.writeInt(opened_fd, newFd).unsafeRunSync()
    // get absolute path

    errnoEnum.`success`
  }

}
