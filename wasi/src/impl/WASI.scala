package impl
import cats.effect.IO
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._

import impl.Types.{
  ciovec_array,
  dircookie,
  exitcode,
  fd,
  filedelta,
  filesize,
  iovec_array,
  siflags,
  size,
  string,
  timestamp,
  u8
}

/**
    @author Javier Cabrera-Arteaga on 2020-03-23
    Follow NodeJS implementation https://github.com/nodejs/wasi/tree/wasi/lib of WASI to implement this
  */
class WASI extends Module {
  override def adapt[Tin, Tout](in: Tin): Tout = ???

  override def args_getImpl(argv: Types.Pointer[Types.Pointer[u8]],
                            argv_buf: Types.Pointer[u8]): Types.errnoEnum.Value = ???

  override def args_sizes_getImpl(argc: Types.Pointer[size],
                                  argv_buf_size: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def environ_getImpl(environ: Types.Pointer[Types.Pointer[u8]],
                               environ_buf: Types.Pointer[u8]): Types.errnoEnum.Value = ???

  override def environ_sizes_getImpl(environc: Types.Pointer[size],
                                     environ_buf_size: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def clock_res_getImpl(id: Types.clockidEnum.Value,
                                 resolution: Types.Pointer[timestamp]): Types.errnoEnum.Value = ???

  override def clock_time_getImpl(id: Types.clockidEnum.Value,
                                  precision: timestamp,
                                  time: Types.Pointer[timestamp]): Types.errnoEnum.Value = ???

  override def fd_adviseImpl(fd: fd,
                             offset: filesize,
                             len: filesize,
                             advice: Types.adviceEnum.Value): Types.errnoEnum.Value = ???

  override def fd_allocateImpl(fd: fd, offset: filesize, len: filesize): Types.errnoEnum.Value = ???

  override def fd_closeImpl(fd: fd): Types.errnoEnum.Value = ???

  override def fd_datasyncImpl(fd: fd): Types.errnoEnum.Value = ???

  override def fd_fdstat_getImpl(fd: fd, stat: Types.Pointer[Types.`fdstat`]): Types.errnoEnum.Value = ???

  override def fd_fdstat_set_flagsImpl(fd: fd, flags: Types.fdflagsFlags.Value): Types.errnoEnum.Value = ???

  override def fd_fdstat_set_rightsImpl(fd: fd,
                                        fs_rights_base: Types.rightsFlags.Value,
                                        fs_rights_inheriting: Types.rightsFlags.Value): Types.errnoEnum.Value = ???

  override def fd_filestat_getImpl(fd: fd, buf: Types.Pointer[Types.`filestat`]): Types.errnoEnum.Value = ???

  override def fd_filestat_set_sizeImpl(fd: fd, size: filesize): Types.errnoEnum.Value = ???

  override def fd_filestat_set_timesImpl(fd: fd,
                                         atim: timestamp,
                                         mtim: timestamp,
                                         fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = ???

  override def fd_preadImpl(fd: fd,
                            iovs: iovec_array,
                            offset: filesize,
                            nread: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def fd_prestat_getImpl(fd: fd, buf: Types.Pointer[Types.`prestat`]): Types.errnoEnum.Value = ???

  override def fd_prestat_dir_nameImpl(fd: fd, path: Types.Pointer[u8], path_len: size): Types.errnoEnum.Value = ???

  override def fd_pwriteImpl(fd: fd,
                             iovs: ciovec_array,
                             offset: filesize,
                             nwritten: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def fd_readImpl(fd: fd, iovs: iovec_array, nread: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def fd_readdirImpl(fd: fd,
                              buf: Types.Pointer[u8],
                              buf_len: size,
                              cookie: dircookie,
                              bufused: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def fd_renumberImpl(fd: fd, to: fd): Types.errnoEnum.Value = ???

  override def fd_seekImpl(fd: fd,
                           offset: filedelta,
                           whence: Types.whenceEnum.Value,
                           newoffset: Types.Pointer[filesize]): Types.errnoEnum.Value = ???

  override def fd_syncImpl(fd: fd): Types.errnoEnum.Value = ???

  override def fd_tellImpl(fd: fd, offset: Types.Pointer[filesize]): Types.errnoEnum.Value = ???

  override def fd_writeImpl(fd: fd, iovs: ciovec_array, nwritten: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def path_create_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = ???

  override def path_filestat_getImpl(fd: fd,
                                     flags: Types.lookupflagsFlags.Value,
                                     path: string,
                                     buf: Types.Pointer[Types.`filestat`]): Types.errnoEnum.Value = ???

  override def path_filestat_set_timesImpl(fd: fd,
                                           flags: Types.lookupflagsFlags.Value,
                                           path: string,
                                           atim: timestamp,
                                           mtim: timestamp,
                                           fst_flags: Types.fstflagsFlags.Value): Types.errnoEnum.Value = ???

  override def path_linkImpl(old_fd: fd,
                             old_flags: Types.lookupflagsFlags.Value,
                             old_path: string,
                             new_fd: fd,
                             new_path: string): Types.errnoEnum.Value = ???

  override def path_openImpl(fd: fd,
                             dirflags: Types.lookupflagsFlags.Value,
                             path: string,
                             oflags: Types.oflagsFlags.Value,
                             fs_rights_base: Types.rightsFlags.Value,
                             fs_rights_inherting: Types.rightsFlags.Value,
                             fdflags: Types.fdflagsFlags.Value,
                             opened_fd: Types.Pointer[fd]): Types.errnoEnum.Value = ???

  override def path_readlinkImpl(fd: fd,
                                 path: string,
                                 buf: Types.Pointer[u8],
                                 buf_len: size,
                                 bufused: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def path_remove_directoryImpl(fd: fd, path: string): Types.errnoEnum.Value = ???

  override def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): Types.errnoEnum.Value = ???

  override def path_symlinkImpl(old_path: string, fd: fd, new_path: string): Types.errnoEnum.Value = ???

  override def path_unlink_fileImpl(fd: fd, path: string): Types.errnoEnum.Value = ???

  override def poll_oneoffImpl(in: Types.Pointer[Types.`subscription`],
                               out: Types.Pointer[Types.`event`],
                               nsubscriptions: size,
                               nevents: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def proc_exitImpl(rval: exitcode): Unit = ???

  override def proc_raiseImpl(sig: Types.signalEnum.Value): Types.errnoEnum.Value = ???

  override def sched_yieldImpl(): Types.errnoEnum.Value = ???

  override def random_getImpl(buf: Types.Pointer[u8], buf_len: size): Types.errnoEnum.Value = ???

  override def sock_recvImpl(fd: fd,
                             ri_data: iovec_array,
                             ri_flags: Types.riflagsFlags.Value,
                             ro_datalen: Types.Pointer[size],
                             ro_flags: Types.Pointer[Types.roflagsFlags.Value]): Types.errnoEnum.Value = ???

  override def sock_sendImpl(fd: fd,
                             si_data: ciovec_array,
                             si_flags: siflags,
                             so_datalen: Types.Pointer[size]): Types.errnoEnum.Value = ???

  override def sock_shutdownImpl(fd: fd, how: Types.sdflagsFlags.Value): Types.errnoEnum.Value = ???

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

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
}
