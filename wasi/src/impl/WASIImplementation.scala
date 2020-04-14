package swam
package wasi

import java.io.{File, FileDescriptor, FileInputStream, FileOutputStream, PrintWriter}
import java.nio.ByteBuffer
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{Files, NoSuchFileException, Paths}

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

    if (!fdMap.contains(fd))
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
    // TODO
    if (fdMap.contains(fd))
      throw new WASIException(errnoEnum.`badf`)

    fdMap.updated(fd, None) // delete the entry

    errnoEnum.`success`
  }

  override def fd_datasyncImpl(fd: fd): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_datasync.id)

    // TODO
    throw new Exception("Not implemented")
    errnoEnum.`fault`
  }

  def printMem() = {
    val f = new PrintWriter(new File("trace.mem"))

    for (i <- 0 until mem.size)
      f.write(s"${(mem.readByte(i).unsafeRunSync())}\n")

    f.close()
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

    stat = stat.copy(`fs_rights_inheriting` = fs_rights_inheriting.id)
    stat = stat.copy(`fs_rights_base` = fs_rights_base.id)

    //fdMap = fdMap.updated(fd, stat)

    // TODO

    errnoEnum.`success`

  }

  override def fd_filestat_getImpl(fd: fd, buf: ptr): Types.errnoEnum.Value = {

    val stat = checkRight(fd, rightsFlags.fd_filestat_get.id)
    // val rstat = parseStats(fdMapFile(fd))

    // TODO
    throw new Exception("Not implemented")

  }

  override def fd_filestat_set_sizeImpl(fd: fd, size: filesize): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_filestat_set_size.id)

    // TODO
    throw new Exception("Not implemented")
    errnoEnum.`fault`
  }

  override def fd_filestat_set_timesImpl(fd: fd,
                                         atim: timestamp,
                                         mtim: timestamp,
                                         fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = {

    // TODO
    throw new Exception("Not implemented")
    errnoEnum.`fault`
  }

  override def fd_preadImpl(fd: fd,
                            iovs: iovec_array,
                            iovsLen: u32,
                            offset: filesize,
                            nread: ptr): Types.errnoEnum.Value = {

    /*val stats = checkRight(fd, rightsFlags.fd_read.id | rightsFlags.fd_write.id)
      var cumul = 0
      val reader = fdMapFile(fd)
      iovs.foreach(iov => {
        val arr = reader.read(0, iov.`buf_len`)
        cumul += iov.`buf_len`
        arr.zipWithIndex.foreach { case (va, i) => iov.`buf`._set(i, va) }
      })

      nread._set(0, cumul)*/
    errnoEnum.`success`
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

    //nread._set(0, 0)
    mem.writeInt(nread, cumul).unsafeRunSync()
    Types.errnoEnum.`success`
  }

  override def fd_readdirImpl(fd: fd,
                              buf: ptr,
                              buf_len: size,
                              cookie: dircookie,
                              bufused: ptr): Types.errnoEnum.Value = {
    // TODO

    throw new Exception("Not implemented")
  }

  override def fd_renumberImpl(fd: fd, to: fd): Types.errnoEnum.Value = {
    throw new Exception("Not implemented")
  }

  override def fd_seekImpl(fd: fd,
                           offset: filedelta,
                           whence: Types.whenceEnum.Value,
                           newoffset: ptr): Types.errnoEnum.Value = {
    val stats = checkRight(fd, rightsFlags.fd_seek.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def fd_syncImpl(fd: fd): Types.errnoEnum.Value = {
    val stats = checkRight(fd, rightsFlags.fd_sync.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def fd_tellImpl(fd: fd, offset: ptr): Types.errnoEnum.Value = {
    val stats = checkRight(fd, rightsFlags.fd_tell.id)

    // TODO
    throw new Exception("Not implemented")

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

    //printMem()
    Types.errnoEnum.`success`
  }

  override def path_create_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = {
    val stats = checkRight(fd, rightsFlags.path_create_directory.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def path_filestat_getImpl(fd: fd,
                                     flags: Types.lookupflagsFlags.Value,
                                     path: string,
                                     buf: ptr): Types.errnoEnum.Value = {

    val stats = checkRight(fd, rightsFlags.fd_filestat_get.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def path_filestat_set_timesImpl(fd: fd,
                                           flags: Types.lookupflagsFlags.Value,
                                           path: string,
                                           atim: timestamp,
                                           mtim: timestamp,
                                           fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = {

    val stats = checkRight(fd, rightsFlags.path_filestat_set_times.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def path_linkImpl(old_fd: fd,
                             old_flags: Types.lookupflagsFlags.Value,
                             old_path: string,
                             new_fd: fd,
                             new_path: string): Types.errnoEnum.Value = {

    val ostats = checkRight(old_fd, rightsFlags.path_link_source.id)
    val nstats = checkRight(old_fd, rightsFlags.path_link_target.id)

    // TODO
    throw new Exception("Not implemented")

  }

  def path_openImpl(fd: fd,
                    dirflags: Types.lookupflagsFlags.Value,
                    path: string,
                    pathLen: Int,
                    oflags: Types.oflagsFlags.Value,
                    fs_rights_base: Types.rightsFlags.Value,
                    fs_rights_inherting: Types.rightsFlags.Value,
                    fdflags: Types.fdflagsFlags.Value,
                    opened_fd: ptr): Types.errnoEnum.Value = {

    val stats = checkRight(fd, rightsFlags.path_open.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def path_readlinkImpl(fd: fd, path: string, buf: ptr, buf_len: size, bufused: ptr): Types.errnoEnum.Value = {

    val stats = checkRight(fd, rightsFlags.path_readlink.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def path_remove_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = {

    val stats = checkRight(fd, rightsFlags.path_remove_directory.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): Types.errnoEnum.Value = {

    val ostats = checkRight(fd, rightsFlags.path_rename_source.id)
    val nstats = checkRight(fd, rightsFlags.path_rename_target.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def path_symlinkImpl(old_path: string, fd: fd, new_path: string): Types.errnoEnum.Value = {

    val stats = checkRight(fd, rightsFlags.path_symlink.id)

    // TODO
    throw new Exception("Not implemented")

    errnoEnum.`fault`

  }

  override def path_unlink_fileImpl(fd: fd, path: string): Types.errnoEnum.Value = {

    val stats = checkRight(fd, rightsFlags.path_unlink_file.id)

    // TODO
    throw new Exception("Not implemented")

  }

  override def poll_oneoffImpl(in: ptr, out: ptr, nsubscriptions: size, nevents: ptr): Types.errnoEnum.Value = {

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
    //Range(0, buf_len).foreach(i => buf._set(i, (255 * Math.random()).toByte))
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
