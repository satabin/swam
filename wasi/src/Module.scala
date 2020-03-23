package swam
package wasi
import Types._
import cats.effect._
trait Module {
  def adapt[Tin, Tout](in: Tin): Tout

  def args_getImpl(argv: Pointer[Pointer[u8]], argv_buf: Pointer[u8]): (errnoEnum.Value)

  def args_get(argv: Int, argv_buf: Int) = {
    val argvAdapted: Pointer[Pointer[u8]] = adapt[Int, Pointer[Pointer[u8]]](argv)
    val argv_bufAdapted: Pointer[u8] = adapt[Int, Pointer[u8]](argv_buf)

    IO(adapt[(errnoEnum.Value), (Int)](args_getImpl(argvAdapted, argv_bufAdapted)))
  }

  def args_sizes_getImpl(argc: Pointer[size], argv_buf_size: Pointer[size]): (errnoEnum.Value)

  def args_sizes_get(argc: Int, argv_buf_size: Int) = {
    val argcAdapted: Pointer[size] = adapt[Int, Pointer[size]](argc)
    val argv_buf_sizeAdapted: Pointer[size] = adapt[Int, Pointer[size]](argv_buf_size)

    IO(adapt[(errnoEnum.Value), (Int)](args_sizes_getImpl(argcAdapted, argv_buf_sizeAdapted)))
  }

  def environ_getImpl(environ: Pointer[Pointer[u8]], environ_buf: Pointer[u8]): (errnoEnum.Value)

  def environ_get(environ: Int, environ_buf: Int) = {
    val environAdapted: Pointer[Pointer[u8]] = adapt[Int, Pointer[Pointer[u8]]](environ)
    val environ_bufAdapted: Pointer[u8] = adapt[Int, Pointer[u8]](environ_buf)

    IO(adapt[(errnoEnum.Value), (Int)](environ_getImpl(environAdapted, environ_bufAdapted)))
  }

  def environ_sizes_getImpl(environc: Pointer[size], environ_buf_size: Pointer[size]): (errnoEnum.Value)

  def environ_sizes_get(environc: Int, environ_buf_size: Int) = {
    val environcAdapted: Pointer[size] = adapt[Int, Pointer[size]](environc)
    val environ_buf_sizeAdapted: Pointer[size] = adapt[Int, Pointer[size]](environ_buf_size)

    IO(adapt[(errnoEnum.Value), (Int)](environ_sizes_getImpl(environcAdapted, environ_buf_sizeAdapted)))
  }

  def clock_res_getImpl(id: clockidEnum.Value, resolution: Pointer[timestamp]): (errnoEnum.Value)

  def clock_res_get(id: Int, resolution: Int) = {
    val idAdapted: clockidEnum.Value = adapt[Int, clockidEnum.Value](id)
    val resolutionAdapted: Pointer[timestamp] = adapt[Int, Pointer[timestamp]](resolution)

    IO(adapt[(errnoEnum.Value), (Int)](clock_res_getImpl(idAdapted, resolutionAdapted)))
  }

  def clock_time_getImpl(id: clockidEnum.Value, precision: timestamp, time: Pointer[timestamp]): (errnoEnum.Value)

  def clock_time_get(id: Int, precision: Long, time: Int) = {
    val idAdapted: clockidEnum.Value = adapt[Int, clockidEnum.Value](id)
    val timeAdapted: Pointer[timestamp] = adapt[Int, Pointer[timestamp]](time)

    IO(adapt[(errnoEnum.Value), (Int)](clock_time_getImpl(idAdapted, precision, timeAdapted)))
  }

  def fd_adviseImpl(fd: fd, offset: filesize, len: filesize, advice: adviceEnum.Value): (errnoEnum.Value)

  def fd_advise(fd: Int, offset: Long, len: Long, advice: Int) = {
    val adviceAdapted: adviceEnum.Value = adapt[Int, adviceEnum.Value](advice)

    IO(adapt[(errnoEnum.Value), (Int)](fd_adviseImpl(fd, offset, len, adviceAdapted)))
  }

  def fd_allocateImpl(fd: fd, offset: filesize, len: filesize): (errnoEnum.Value)

  def fd_allocate(fd: Int, offset: Long, len: Long) = {

    IO(adapt[(errnoEnum.Value), (Int)](fd_allocateImpl(fd, offset, len)))
  }

  def fd_closeImpl(fd: fd): (errnoEnum.Value)

  def fd_close(fd: Int) = {

    IO(adapt[(errnoEnum.Value), (Int)](fd_closeImpl(fd)))
  }

  def fd_datasyncImpl(fd: fd): (errnoEnum.Value)

  def fd_datasync(fd: Int) = {

    IO(adapt[(errnoEnum.Value), (Int)](fd_datasyncImpl(fd)))
  }

  def fd_fdstat_getImpl(fd: fd, stat: Pointer[fdstat]): (errnoEnum.Value)

  def fd_fdstat_get(fd: Int, stat: Int) = {
    val statAdapted: Pointer[fdstat] = adapt[Int, Pointer[fdstat]](stat)

    IO(adapt[(errnoEnum.Value), (Int)](fd_fdstat_getImpl(fd, statAdapted)))
  }

  def fd_fdstat_set_flagsImpl(fd: fd, flags: fdflagsFlags.Value): (errnoEnum.Value)

  def fd_fdstat_set_flags(fd: Int, flags: Int) = {
    val flagsAdapted: fdflagsFlags.Value = adapt[Int, fdflagsFlags.Value](flags)

    IO(adapt[(errnoEnum.Value), (Int)](fd_fdstat_set_flagsImpl(fd, flagsAdapted)))
  }

  def fd_fdstat_set_rightsImpl(fd: fd,
                               fs_rights_base: rightsFlags.Value,
                               fs_rights_inheriting: rightsFlags.Value): (errnoEnum.Value)

  def fd_fdstat_set_rights(fd: Int, fs_rights_base: Int, fs_rights_inheriting: Int) = {
    val fs_rights_baseAdapted: rightsFlags.Value = adapt[Int, rightsFlags.Value](fs_rights_base)
    val fs_rights_inheritingAdapted: rightsFlags.Value = adapt[Int, rightsFlags.Value](fs_rights_inheriting)

    IO(
      adapt[(errnoEnum.Value), (Int)](fd_fdstat_set_rightsImpl(fd, fs_rights_baseAdapted, fs_rights_inheritingAdapted)))
  }

  def fd_filestat_getImpl(fd: fd, buf: Pointer[filestat]): (errnoEnum.Value)

  def fd_filestat_get(fd: Int, buf: Int) = {
    val bufAdapted: Pointer[filestat] = adapt[Int, Pointer[filestat]](buf)

    IO(adapt[(errnoEnum.Value), (Int)](fd_filestat_getImpl(fd, bufAdapted)))
  }

  def fd_filestat_set_sizeImpl(fd: fd, size: filesize): (errnoEnum.Value)

  def fd_filestat_set_size(fd: Int, size: Long) = {

    IO(adapt[(errnoEnum.Value), (Int)](fd_filestat_set_sizeImpl(fd, size)))
  }

  def fd_filestat_set_timesImpl(fd: fd,
                                atim: timestamp,
                                mtim: timestamp,
                                fst_flags: fstflagsFlags.Value): (errnoEnum.Value)

  def fd_filestat_set_times(fd: Int, atim: Long, mtim: Long, fst_flags: Int) = {
    val fst_flagsAdapted: fstflagsFlags.Value = adapt[Int, fstflagsFlags.Value](fst_flags)

    IO(adapt[(errnoEnum.Value), (Int)](fd_filestat_set_timesImpl(fd, atim, mtim, fst_flagsAdapted)))
  }

  def fd_preadImpl(fd: fd, iovs: iovec_array, iovsLen: u32, offset: filesize, nread: Pointer[size]): (errnoEnum.Value)

  def fd_pread(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nread: Int) = {
    val iovsAdapted: iovec_array = adapt[Int, iovec_array](iovs)
    val nreadAdapted: Pointer[size] = adapt[Int, Pointer[size]](nread)

    IO(adapt[(errnoEnum.Value), (Int)](fd_preadImpl(fd, iovsAdapted, iovsLen, offset, nreadAdapted)))
  }

  def fd_prestat_getImpl(fd: fd, buf: Pointer[prestat]): (errnoEnum.Value)

  def fd_prestat_get(fd: Int, buf: Int) = {
    val bufAdapted: Pointer[prestat] = adapt[Int, Pointer[prestat]](buf)

    IO(adapt[(errnoEnum.Value), (Int)](fd_prestat_getImpl(fd, bufAdapted)))
  }

  def fd_prestat_dir_nameImpl(fd: fd, path: Pointer[u8], path_len: size): (errnoEnum.Value)

  def fd_prestat_dir_name(fd: Int, path: Int, path_len: Int) = {
    val pathAdapted: Pointer[u8] = adapt[Int, Pointer[u8]](path)

    IO(adapt[(errnoEnum.Value), (Int)](fd_prestat_dir_nameImpl(fd, pathAdapted, path_len)))
  }

  def fd_pwriteImpl(fd: fd,
                    iovs: ciovec_array,
                    iovsLen: u32,
                    offset: filesize,
                    nwritten: Pointer[size]): (errnoEnum.Value)

  def fd_pwrite(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nwritten: Int) = {
    val iovsAdapted: ciovec_array = adapt[Int, ciovec_array](iovs)
    val nwrittenAdapted: Pointer[size] = adapt[Int, Pointer[size]](nwritten)

    IO(adapt[(errnoEnum.Value), (Int)](fd_pwriteImpl(fd, iovsAdapted, iovsLen, offset, nwrittenAdapted)))
  }

  def fd_readImpl(fd: fd, iovs: iovec_array, iovsLen: u32, nread: Pointer[size]): (errnoEnum.Value)

  def fd_read(fd: Int, iovs: Int, iovsLen: Int, nread: Int) = {
    val iovsAdapted: iovec_array = adapt[Int, iovec_array](iovs)
    val nreadAdapted: Pointer[size] = adapt[Int, Pointer[size]](nread)

    IO(adapt[(errnoEnum.Value), (Int)](fd_readImpl(fd, iovsAdapted, iovsLen, nreadAdapted)))
  }

  def fd_readdirImpl(fd: fd,
                     buf: Pointer[u8],
                     buf_len: size,
                     cookie: dircookie,
                     bufused: Pointer[size]): (errnoEnum.Value)

  def fd_readdir(fd: Int, buf: Int, buf_len: Int, cookie: Long, bufused: Int) = {
    val bufAdapted: Pointer[u8] = adapt[Int, Pointer[u8]](buf)
    val bufusedAdapted: Pointer[size] = adapt[Int, Pointer[size]](bufused)

    IO(adapt[(errnoEnum.Value), (Int)](fd_readdirImpl(fd, bufAdapted, buf_len, cookie, bufusedAdapted)))
  }

  def fd_renumberImpl(fd: fd, to: fd): (errnoEnum.Value)

  def fd_renumber(fd: Int, to: Int) = {

    IO(adapt[(errnoEnum.Value), (Int)](fd_renumberImpl(fd, to)))
  }

  def fd_seekImpl(fd: fd, offset: filedelta, whence: whenceEnum.Value, newoffset: Pointer[filesize]): (errnoEnum.Value)

  def fd_seek(fd: Int, offset: Long, whence: Int, newoffset: Int) = {
    val whenceAdapted: whenceEnum.Value = adapt[Int, whenceEnum.Value](whence)
    val newoffsetAdapted: Pointer[filesize] = adapt[Int, Pointer[filesize]](newoffset)

    IO(adapt[(errnoEnum.Value), (Int)](fd_seekImpl(fd, offset, whenceAdapted, newoffsetAdapted)))
  }

  def fd_syncImpl(fd: fd): (errnoEnum.Value)

  def fd_sync(fd: Int) = {

    IO(adapt[(errnoEnum.Value), (Int)](fd_syncImpl(fd)))
  }

  def fd_tellImpl(fd: fd, offset: Pointer[filesize]): (errnoEnum.Value)

  def fd_tell(fd: Int, offset: Int) = {
    val offsetAdapted: Pointer[filesize] = adapt[Int, Pointer[filesize]](offset)

    IO(adapt[(errnoEnum.Value), (Int)](fd_tellImpl(fd, offsetAdapted)))
  }

  def fd_writeImpl(fd: fd, iovs: ciovec_array, iovsLen: u32, nwritten: Pointer[size]): (errnoEnum.Value)

  def fd_write(fd: Int, iovs: Int, iovsLen: Int, nwritten: Int) = {
    val iovsAdapted: ciovec_array = adapt[Int, ciovec_array](iovs)
    val nwrittenAdapted: Pointer[size] = adapt[Int, Pointer[size]](nwritten)

    IO(adapt[(errnoEnum.Value), (Int)](fd_writeImpl(fd, iovsAdapted, iovsLen, nwrittenAdapted)))
  }

  def path_create_directoryImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_create_directory(fd: Int, path: Int) = {
    val pathAdapted: String = adapt[Int, String](path)

    IO(adapt[(errnoEnum.Value), (Int)](path_create_directoryImpl(fd, pathAdapted)))
  }

  def path_filestat_getImpl(fd: fd,
                            flags: lookupflagsFlags.Value,
                            path: string,
                            buf: Pointer[filestat]): (errnoEnum.Value)

  def path_filestat_get(fd: Int, flags: Int, path: Int, buf: Int) = {
    val flagsAdapted: lookupflagsFlags.Value = adapt[Int, lookupflagsFlags.Value](flags)
    val pathAdapted: String = adapt[Int, String](path)
    val bufAdapted: Pointer[filestat] = adapt[Int, Pointer[filestat]](buf)

    IO(adapt[(errnoEnum.Value), (Int)](path_filestat_getImpl(fd, flagsAdapted, pathAdapted, bufAdapted)))
  }

  def path_filestat_set_timesImpl(fd: fd,
                                  flags: lookupflagsFlags.Value,
                                  path: string,
                                  atim: timestamp,
                                  mtim: timestamp,
                                  fst_flags: fstflagsFlags.Value): (errnoEnum.Value)

  def path_filestat_set_times(fd: Int, flags: Int, path: Int, atim: Long, mtim: Long, fst_flags: Int) = {
    val flagsAdapted: lookupflagsFlags.Value = adapt[Int, lookupflagsFlags.Value](flags)
    val pathAdapted: String = adapt[Int, String](path)
    val fst_flagsAdapted: fstflagsFlags.Value = adapt[Int, fstflagsFlags.Value](fst_flags)

    IO(
      adapt[(errnoEnum.Value), (Int)](
        path_filestat_set_timesImpl(fd, flagsAdapted, pathAdapted, atim, mtim, fst_flagsAdapted)))
  }

  def path_linkImpl(old_fd: fd,
                    old_flags: lookupflagsFlags.Value,
                    old_path: string,
                    new_fd: fd,
                    new_path: string): (errnoEnum.Value)

  def path_link(old_fd: Int, old_flags: Int, old_path: Int, new_fd: Int, new_path: Int) = {
    val old_flagsAdapted: lookupflagsFlags.Value = adapt[Int, lookupflagsFlags.Value](old_flags)
    val old_pathAdapted: String = adapt[Int, String](old_path)
    val new_pathAdapted: String = adapt[Int, String](new_path)

    IO(
      adapt[(errnoEnum.Value), (Int)](
        path_linkImpl(old_fd, old_flagsAdapted, old_pathAdapted, new_fd, new_pathAdapted)))
  }

  def path_openImpl(fd: fd,
                    dirflags: lookupflagsFlags.Value,
                    path: string,
                    oflags: oflagsFlags.Value,
                    fs_rights_base: rightsFlags.Value,
                    fs_rights_inherting: rightsFlags.Value,
                    fdflags: fdflagsFlags.Value,
                    opened_fd: Pointer[fd]): (errnoEnum.Value)

  def path_open(fd: Int,
                dirflags: Int,
                path: Int,
                oflags: Int,
                fs_rights_base: Int,
                fs_rights_inherting: Int,
                fdflags: Int,
                opened_fd: Int) = {
    val dirflagsAdapted: lookupflagsFlags.Value = adapt[Int, lookupflagsFlags.Value](dirflags)
    val pathAdapted: String = adapt[Int, String](path)
    val oflagsAdapted: oflagsFlags.Value = adapt[Int, oflagsFlags.Value](oflags)
    val fs_rights_baseAdapted: rightsFlags.Value = adapt[Int, rightsFlags.Value](fs_rights_base)
    val fs_rights_inhertingAdapted: rightsFlags.Value = adapt[Int, rightsFlags.Value](fs_rights_inherting)
    val fdflagsAdapted: fdflagsFlags.Value = adapt[Int, fdflagsFlags.Value](fdflags)
    val opened_fdAdapted: Pointer[fd] = adapt[Int, Pointer[fd]](opened_fd)

    IO(
      adapt[(errnoEnum.Value), (Int)](
        path_openImpl(fd,
                      dirflagsAdapted,
                      pathAdapted,
                      oflagsAdapted,
                      fs_rights_baseAdapted,
                      fs_rights_inhertingAdapted,
                      fdflagsAdapted,
                      opened_fdAdapted)))
  }

  def path_readlinkImpl(fd: fd,
                        path: string,
                        buf: Pointer[u8],
                        buf_len: size,
                        bufused: Pointer[size]): (errnoEnum.Value)

  def path_readlink(fd: Int, path: Int, buf: Int, buf_len: Int, bufused: Int) = {
    val pathAdapted: String = adapt[Int, String](path)
    val bufAdapted: Pointer[u8] = adapt[Int, Pointer[u8]](buf)
    val bufusedAdapted: Pointer[size] = adapt[Int, Pointer[size]](bufused)

    IO(adapt[(errnoEnum.Value), (Int)](path_readlinkImpl(fd, pathAdapted, bufAdapted, buf_len, bufusedAdapted)))
  }

  def path_remove_directoryImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_remove_directory(fd: Int, path: Int) = {
    val pathAdapted: String = adapt[Int, String](path)

    IO(adapt[(errnoEnum.Value), (Int)](path_remove_directoryImpl(fd, pathAdapted)))
  }

  def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): (errnoEnum.Value)

  def path_rename(fd: Int, old_path: Int, new_fd: Int, new_path: Int) = {
    val old_pathAdapted: String = adapt[Int, String](old_path)
    val new_pathAdapted: String = adapt[Int, String](new_path)

    IO(adapt[(errnoEnum.Value), (Int)](path_renameImpl(fd, old_pathAdapted, new_fd, new_pathAdapted)))
  }

  def path_symlinkImpl(old_path: string, fd: fd, new_path: string): (errnoEnum.Value)

  def path_symlink(old_path: Int, fd: Int, new_path: Int) = {
    val old_pathAdapted: String = adapt[Int, String](old_path)
    val new_pathAdapted: String = adapt[Int, String](new_path)

    IO(adapt[(errnoEnum.Value), (Int)](path_symlinkImpl(old_pathAdapted, fd, new_pathAdapted)))
  }

  def path_unlink_fileImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_unlink_file(fd: Int, path: Int) = {
    val pathAdapted: String = adapt[Int, String](path)

    IO(adapt[(errnoEnum.Value), (Int)](path_unlink_fileImpl(fd, pathAdapted)))
  }

  def poll_oneoffImpl(in: Pointer[subscription],
                      out: Pointer[event],
                      nsubscriptions: size,
                      nevents: Pointer[size]): (errnoEnum.Value)

  def poll_oneoff(in: Int, out: Int, nsubscriptions: Int, nevents: Int) = {
    val inAdapted: Pointer[subscription] = adapt[Int, Pointer[subscription]](in)
    val outAdapted: Pointer[event] = adapt[Int, Pointer[event]](out)
    val neventsAdapted: Pointer[size] = adapt[Int, Pointer[size]](nevents)

    IO(adapt[(errnoEnum.Value), (Int)](poll_oneoffImpl(inAdapted, outAdapted, nsubscriptions, neventsAdapted)))
  }

  def proc_exitImpl(rval: exitcode) = {}

  def proc_exit(rval: Int) = {

    IO(proc_exitImpl(rval))
  }

  def proc_raiseImpl(sig: signalEnum.Value): (errnoEnum.Value)

  def proc_raise(sig: Int) = {
    val sigAdapted: signalEnum.Value = adapt[Int, signalEnum.Value](sig)

    IO(adapt[(errnoEnum.Value), (Int)](proc_raiseImpl(sigAdapted)))
  }

  def sched_yieldImpl(): (errnoEnum.Value)

  def sched_yield() = {

    IO(adapt[(errnoEnum.Value), (Int)](sched_yieldImpl()))
  }

  def random_getImpl(buf: Pointer[u8], buf_len: size): (errnoEnum.Value)

  def random_get(buf: Int, buf_len: Int) = {
    val bufAdapted: Pointer[u8] = adapt[Int, Pointer[u8]](buf)

    IO(adapt[(errnoEnum.Value), (Int)](random_getImpl(bufAdapted, buf_len)))
  }

  def sock_recvImpl(fd: fd,
                    ri_data: iovec_array,
                    ri_dataLen: u32,
                    ri_flags: riflagsFlags.Value,
                    ro_datalen: Pointer[size],
                    ro_flags: Pointer[roflagsFlags.Value]): (errnoEnum.Value)

  def sock_recv(fd: Int, ri_data: Int, ri_dataLen: Int, ri_flags: Int, ro_datalen: Int, ro_flags: Int) = {
    val ri_dataAdapted: iovec_array = adapt[Int, iovec_array](ri_data)
    val ri_flagsAdapted: riflagsFlags.Value = adapt[Int, riflagsFlags.Value](ri_flags)
    val ro_datalenAdapted: Pointer[size] = adapt[Int, Pointer[size]](ro_datalen)
    val ro_flagsAdapted: Pointer[roflagsFlags.Value] = adapt[Int, Pointer[roflagsFlags.Value]](ro_flags)

    IO(
      adapt[(errnoEnum.Value), (Int)](
        sock_recvImpl(fd, ri_dataAdapted, ri_dataLen, ri_flagsAdapted, ro_datalenAdapted, ro_flagsAdapted)))
  }

  def sock_sendImpl(fd: fd,
                    si_data: ciovec_array,
                    si_dataLen: u32,
                    si_flags: siflags,
                    so_datalen: Pointer[size]): (errnoEnum.Value)

  def sock_send(fd: Int, si_data: Int, si_dataLen: Int, si_flags: Int, so_datalen: Int) = {
    val si_dataAdapted: ciovec_array = adapt[Int, ciovec_array](si_data)
    val si_flagsAdapted: Short = adapt[Int, Short](si_flags)
    val so_datalenAdapted: Pointer[size] = adapt[Int, Pointer[size]](so_datalen)

    IO(
      adapt[(errnoEnum.Value), (Int)](
        sock_sendImpl(fd, si_dataAdapted, si_dataLen, si_flagsAdapted, so_datalenAdapted)))
  }

  def sock_shutdownImpl(fd: fd, how: sdflagsFlags.Value): (errnoEnum.Value)

  def sock_shutdown(fd: Int, how: Int) = {
    val howAdapted: sdflagsFlags.Value = adapt[Int, sdflagsFlags.Value](how)

    IO(adapt[(errnoEnum.Value), (Int)](sock_shutdownImpl(fd, howAdapted)))
  }

}
