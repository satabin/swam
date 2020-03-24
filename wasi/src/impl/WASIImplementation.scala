package swam
package wasi

import java.io.{File, FileDescriptor, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import cats.effect.IO
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._
import swam.wasi.Types._

/**
    @author Javier Cabrera-Arteaga on 2020-03-23
    Follow NodeJS implementation https://github.com/nodejs/wasi/tree/wasi/lib of WASI to implement this
  */
class WASIImplementation(override var mem: ByteBuffer = null, override val args: Seq[String]) extends Module {

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  val WASI_STDIN = 0
  val WASI_STDOUT = 1
  val WASI_STDERR = 2

  var fdMap = Map[fd, `fdstat`](
    WASI_STDIN -> `fdstat`(-1, -1, -1, -1),
    WASI_STDOUT -> `fdstat`(-1, -1, -1, -1),
    WASI_STDERR -> `fdstat`(-1, -1, -1, -1)
  )

  var fdMapFile = Map[fd, FileDescriptor](
    WASI_STDIN -> FileDescriptor.in,
    WASI_STDOUT -> FileDescriptor.out,
    WASI_STDERR -> FileDescriptor.err
  )

  class WASIException(val errno: errnoEnum.Value) extends Exception

  /**
    * Returns stat numbers: filetype rightsBase rightsInheriting
    *
    * @param fd
    */
  def parseStats(fd: FileDescriptor) = {
    (filetypeEnum.`regular_file`, regular_file_base, RIGHTS_REGULAR_FILE_INHERITING)
  }

  def checkRight(fd: fd, rights: Int): `fdstat` = {

    if (!fdMap.contains(fd))
      throw new WASIException(errnoEnum.`badf`)

    if (rights != 0 && (fdMap(fd).`fs_rights_base` & rights) == 0)
      throw new WASIException(errnoEnum.`perm`)

    var st = fdMap(fd)
    val file = fdMapFile(fd)

    val stats = parseStats(file)
    if (st.`fs_filetype` == -1) {
      st = st.copy(`fs_filetype` = stats._1.id.toByte)
    }

    if (st.`fs_rights_base` == -1)
      st = st.copy(`fs_rights_base` = stats._2, `fs_rights_inheriting` = stats._3)

    st
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

  def imports() = {
    Imports[IO](
      TCMap[String, AsIsIO](
        "wasi_unstable" -> TCMap[String, AsIIO](
          "args_get" -> args_get _,
          "args_sizes_get" -> args_sizes_get _,
          "environ_get" -> environ_get _,
          "environ_sizes_get" -> environ_sizes_get _,
          "clock_res_get" -> clock_res_get _,
          "clock_time_get" -> clock_time_get _,
          "fd_advise" -> fd_advise _,
          "fd_allocate" -> fd_allocate _,
          "fd_close" -> fd_close _,
          "fd_datasync" -> fd_datasync _,
          "fd_fdstat_get" -> fd_fdstat_get _,
          "fd_fdstat_set_flags" -> fd_fdstat_set_flags _,
          "fd_fdstat_set_rights" -> fd_fdstat_set_rights _,
          "fd_filestat_get" -> fd_filestat_get _,
          "fd_filestat_set_size" -> fd_filestat_set_size _,
          "fd_filestat_set_times" -> fd_filestat_set_times _,
          "fd_pread" -> fd_pread _,
          "fd_prestat_get" -> fd_prestat_get _,
          "fd_prestat_dir_name" -> fd_prestat_dir_name _,
          "fd_pwrite" -> fd_pwrite _,
          "fd_read" -> fd_read _,
          "fd_readdir" -> fd_readdir _,
          "fd_renumber" -> fd_renumber _,
          "fd_seek" -> fd_seek _,
          "fd_sync" -> fd_sync _,
          "fd_tell" -> fd_tell _,
          "fd_write" -> fd_write _,
          "path_create_directory" -> path_create_directory _,
          "path_filestat_get" -> path_filestat_get _,
          "path_filestat_set_times" -> path_filestat_set_times _,
          "path_link" -> path_link _,
          "path_open" -> path_open _,
          "path_readlink" -> path_readlink _,
          "path_remove_directory" -> path_remove_directory _,
          "path_rename" -> path_rename _,
          "path_symlink" -> path_symlink _,
          "path_unlink_file" -> path_unlink_file _,
          "poll_oneoff" -> poll_oneoff _,
          "proc_exit" -> proc_exit _,
          "proc_raise" -> proc_raise _,
          "sched_yield" -> sched_yield _,
          "random_get" -> random_get _,
          "sock_recv" -> sock_recv _,
          "sock_send" -> sock_send _,
          "sock_shutdown" -> sock_shutdown _
        )
      )
    )
  }

  override def args_getImpl(argv: Pointer[Pointer[u8]], argv_buf: Pointer[u8]): Types.errnoEnum.Value = {
    var coffset = argv.offset
    var offset = argv_buf.offset

    args.foreach(arg => {
      mem.putInt(coffset, offset)
      coffset += 4
      val bytes = new Array[Byte](arg.length + 1)
      val oldPos = mem.position()
      mem.position(offset)
      mem.put(arg.getBytes(), 0, bytes.length)
      mem.position(oldPos)
      offset += bytes.length
    })
    errnoEnum.`success`
  }

  override def args_sizes_getImpl(argc: Pointer[size], argv_buf_size: Pointer[size]): Types.errnoEnum.Value = {
    mem.putInt(argc.offset, args.length)
    mem.putInt(argv_buf_size.offset, args.map(t => t.length).sum)
    errnoEnum.`success`
  }

  override def environ_getImpl(environ: Pointer[Pointer[u8]], environ_buf: Pointer[u8]): Types.errnoEnum.Value = {
    var coffset = environ.offset
    var offset = environ_buf.offset

    System.getenv().forEach {
      case (key, value) => {
        mem.putInt(coffset, offset)
        coffset += 4
        val bytes = new Array[Byte](s"$key=$value".getBytes().length + 1)
        val oldPos = mem.position()
        mem.position(offset)
        mem.put(s"$key=$value".getBytes(), 0, bytes.length)
        mem.position(oldPos)
        offset += bytes.length
      }
    }
    errnoEnum.`success`

  }

  override def environ_sizes_getImpl(environc: Pointer[size],
                                     environ_buf_size: Pointer[size]): Types.errnoEnum.Value = {
    var cumul = 0
    System.getenv().forEach {
      case (key, value) => {
        val processed = s"$key=$value"
        cumul += processed.getBytes().length + 1
      }
    }

    mem.putInt(environc.offset, System.getenv().size())
    mem.putInt(environ_buf_size.offset, cumul)
    errnoEnum.`success`
  }

  override def clock_res_getImpl(id: Types.clockidEnum.Value, resolution: Pointer[timestamp]): Types.errnoEnum.Value = {
    mem.putLong(resolution.offset, 0L)
    errnoEnum.`success`
  }

  override def clock_time_getImpl(id: Types.clockidEnum.Value,
                                  precision: timestamp,
                                  time: Pointer[timestamp]): Types.errnoEnum.Value = {
    val nowVal = now(id)

    if (nowVal < 0)
      throw new WASIException(errnoEnum.`inval`)

    mem.putLong(time.offset, nowVal)

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
    fdMap = fdMap.removed(fd)
    fdMapFile = fdMapFile.removed(fd)
    errnoEnum.`success`
  }

  override def fd_datasyncImpl(fd: fd): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_datasync.id)
    // TODO fdatasyncSync
    errnoEnum.`fault`
  }

  override def fd_fdstat_getImpl(fd: fd, stat: Types.`fdstat`, offset: Int): Types.errnoEnum.Value = {

    try {
      val st = checkRight(fd, 0)
      st.write(offset, mem)
      Types.errnoEnum.`success`
    } catch {
      case x: WASIException => x.errno
    }

  }

  override def fd_fdstat_set_flagsImpl(fd: fd, flags: Types.fdflagsFlags.Value): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_fdstat_set_flags.id)
    errnoEnum.`nosys`
  }

  override def fd_fdstat_set_rightsImpl(fd: fd,
                                        fs_rights_base: Types.rightsFlags.Value,
                                        fs_rights_inheriting: Types.rightsFlags.Value): Types.errnoEnum.Value = {
    try {
      var stat = checkRight(fd, 0)
      val nrb = stat.`fs_rights_base` | fs_rights_base.id

      if (nrb > stat.`fs_rights_base`)
        errnoEnum.`perm`

      val nri = stat.`fs_rights_inheriting` | fs_rights_inheriting.id

      if (nri > stat.`fs_rights_inheriting`)
        errnoEnum.`perm`

      stat = stat.copy(`fs_rights_inheriting` = fs_rights_inheriting.id)
      stat = stat.copy(`fs_rights_base` = fs_rights_base.id)

      fdMap = fdMap.updated(fd, stat)

      errnoEnum.`success`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_filestat_getImpl(fd: fd, buf: Pointer[Types.`filestat`]): Types.errnoEnum.Value = {

    try {
      val stat = checkRight(fd, rightsFlags.fd_filestat_get.id)
      val rstat = parseStats(fdMapFile(fd))

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }

  }

  override def fd_filestat_set_sizeImpl(fd: fd, size: filesize): Types.errnoEnum.Value = {
    checkRight(fd, rightsFlags.fd_filestat_set_size.id)
    // TODO
    errnoEnum.`fault`
  }

  override def fd_filestat_set_timesImpl(fd: fd,
                                         atim: timestamp,
                                         mtim: timestamp,
                                         fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = {
    // TODO
    errnoEnum.`fault`
  }

  override def fd_preadImpl(fd: fd,
                            iovs: iovec_array,
                            iovsLen: u32,
                            offset: filesize,
                            nread: Pointer[size]): Types.errnoEnum.Value = {

    try {
      val stats = checkRight(fd, rightsFlags.fd_read.id | rightsFlags.fd_write.id)
      var cumul = 0
      val writer = new FileInputStream(fdMapFile(fd))
      iovs.foreach(iov => {
        val arr = new Array[Byte](iov.`buf_len`)
        writer.read(arr, 0, arr.length)
        cumul += iov.`buf_len`
        arr.zipWithIndex.foreach { case (va, i) => iov.`buf`._set(i, va) }
      })

      nread._set(0, cumul)
      errnoEnum.`success`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_prestat_getImpl(fd: fd, buf: Pointer[Types.`prestat`]): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, 0)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_prestat_dir_nameImpl(fd: fd, path: Pointer[u8], path_len: size): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, 0)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_pwriteImpl(fd: fd,
                             iovs: ciovec_array,
                             iovsLen: u32,
                             offset: filesize,
                             nwritten: Pointer[size]): Types.errnoEnum.Value = ???

  override def fd_readImpl(fd: fd, iovs: iovec_array, iovsLen: u32, nread: Pointer[size]): Types.errnoEnum.Value = {
    try {
      checkRight(fd, rightsFlags.fd_read.id)
      var cumul = 0
      val writer = new FileInputStream(fdMapFile(fd))
      iovs.foreach(iov => {
        val arr = new Array[Byte](iov.`buf_len`)
        writer.read(arr, 0, arr.length)
        cumul += iov.`buf_len`
        arr.zipWithIndex.foreach { case (va, i) => iov.`buf`._set(i, va) }
      })

      nread._set(0, cumul)

      Types.errnoEnum.`success`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_readdirImpl(fd: fd,
                              buf: Pointer[u8],
                              buf_len: size,
                              cookie: dircookie,
                              bufused: Pointer[size]): Types.errnoEnum.Value = ???

  override def fd_renumberImpl(fd: fd, to: fd): Types.errnoEnum.Value = ???

  override def fd_seekImpl(fd: fd,
                           offset: filedelta,
                           whence: Types.whenceEnum.Value,
                           newoffset: Pointer[filesize]): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.fd_seek.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_syncImpl(fd: fd): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.fd_sync.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_tellImpl(fd: fd, offset: Pointer[filesize]): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.fd_tell.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def fd_writeImpl(fd: fd,
                            iovs: ciovec_array,
                            iovsLen: u32,
                            nwritten: Pointer[size]): Types.errnoEnum.Value = {

    try {
      checkRight(fd, rightsFlags.fd_write.id)
      var cumul = 0
      val writer = new FileOutputStream(fdMapFile(fd))
      iovs.foreach(iov => {
        writer.write(Range(0, iov.`buf_len`).map(i => iov.`buf`._get(i)).toArray, 0, iov.`buf_len`)
        cumul += iov.`buf_len`
      })

      nwritten._set(0, cumul)

      Types.errnoEnum.`success`
    } catch {
      case x: WASIException => x.errno
    }

  }

  override def path_create_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.path_create_directory.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_filestat_getImpl(fd: fd,
                                     flags: Types.lookupflagsFlags.Value,
                                     path: string,
                                     buf: Pointer[Types.`filestat`]): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.fd_filestat_get.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_filestat_set_timesImpl(fd: fd,
                                           flags: Types.lookupflagsFlags.Value,
                                           path: string,
                                           atim: timestamp,
                                           mtim: timestamp,
                                           fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.path_filestat_set_times.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_linkImpl(old_fd: fd,
                             old_flags: Types.lookupflagsFlags.Value,
                             old_path: string,
                             new_fd: fd,
                             new_path: string): Types.errnoEnum.Value = {
    try {
      val ostats = checkRight(old_fd, rightsFlags.path_link_source.id)
      val nstats = checkRight(old_fd, rightsFlags.path_link_target.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_openImpl(fd: fd,
                             dirflags: Types.lookupflagsFlags.Value,
                             path: string,
                             pathLen: Int,
                             oflags: Types.oflagsFlags.Value,
                             fs_rights_base: Types.rightsFlags.Value,
                             fs_rights_inherting: Types.rightsFlags.Value,
                             fdflags: Types.fdflagsFlags.Value,
                             opened_fd: Pointer[fd]): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.path_open.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_readlinkImpl(fd: fd,
                                 path: string,
                                 buf: Pointer[u8],
                                 buf_len: size,
                                 bufused: Pointer[size]): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.path_readlink.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_remove_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.path_remove_directory.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): Types.errnoEnum.Value = {
    try {
      val ostats = checkRight(fd, rightsFlags.path_rename_source.id)
      val nstats = checkRight(fd, rightsFlags.path_rename_target.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_symlinkImpl(old_path: string, fd: fd, new_path: string): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.path_symlink.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def path_unlink_fileImpl(fd: fd, path: string): Types.errnoEnum.Value = {
    try {
      val stats = checkRight(fd, rightsFlags.path_unlink_file.id)

      // TODO

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
  }

  override def poll_oneoffImpl(in: Pointer[Types.`subscription`],
                               out: Pointer[Types.`event`],
                               nsubscriptions: size,
                               nevents: Pointer[size]): Types.errnoEnum.Value = {
    try {

      errnoEnum.`fault`
    } catch {
      case x: WASIException => x.errno
    }
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

  override def random_getImpl(buf: Pointer[u8], buf_len: size): Types.errnoEnum.Value = {
    Range(0, buf_len).foreach(i => buf._set(i, (255 * Math.random()).toByte))
    errnoEnum.`success`
  }

  override def sock_recvImpl(fd: fd,
                             ri_data: iovec_array,
                             ri_dataLen: u32,
                             ri_flags: Types.riflagsFlags.Value,
                             ro_datalen: Pointer[size],
                             ro_flags: Pointer[Types.roflagsFlags.Value]): Types.errnoEnum.Value = {
    errnoEnum.`nosys`
  }

  override def sock_sendImpl(fd: fd,
                             si_data: ciovec_array,
                             si_dataLen: u32,
                             si_flags: siflags,
                             so_datalen: Pointer[size]): Types.errnoEnum.Value = {
    errnoEnum.`nosys`
  }

  override def sock_shutdownImpl(fd: fd, how: Types.sdflagsFlags.Value): Types.errnoEnum.Value = {
    errnoEnum.`nosys`
  }
}
