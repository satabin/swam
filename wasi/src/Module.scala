package swam
package wasi
import java.nio.ByteBuffer

import Types._
import cats.effect._
trait Module {

  var mem: ByteBuffer
  val args: Seq[String]

  def getString(buffer: ByteBuffer, offset: Int, length: Int) = {
    val results = new Array[Byte](length)
    val old = mem.position()
    mem.position(offset)
    val str = String.valueOf(buffer.get(results, 0, length))
    mem.position(old)
    str
  }

  def args_getImpl(argv: Pointer[Pointer[u8]], argv_buf: Pointer[u8]): (errnoEnum.Value)

  def args_get(argv: Int, argv_buf: Int) = {
    val argvAdapted: Pointer[Pointer[u8]] = new Pointer[Pointer[u8]](
      argv,
      (i) => new Pointer[u8](i, (i) => mem.get(i), (i, r) => mem.put(i, `r`)),
      (i, r) => mem.putInt(i, `r`.offset))
    val argv_bufAdapted: Pointer[u8] = new Pointer[u8](argv_buf, (i) => mem.get(i), (i, r) => mem.put(i, `r`))

    IO(args_getImpl(argvAdapted, argv_bufAdapted).id)
  }

  def args_sizes_getImpl(argc: Pointer[size], argv_buf_size: Pointer[size]): (errnoEnum.Value)

  def args_sizes_get(argc: Int, argv_buf_size: Int) = {
    val argcAdapted: Pointer[size] = new Pointer[size](argc, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))
    val argv_buf_sizeAdapted: Pointer[size] =
      new Pointer[size](argv_buf_size, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(args_sizes_getImpl(argcAdapted, argv_buf_sizeAdapted).id)
  }

  def environ_getImpl(environ: Pointer[Pointer[u8]], environ_buf: Pointer[u8]): (errnoEnum.Value)

  def environ_get(environ: Int, environ_buf: Int) = {
    val environAdapted: Pointer[Pointer[u8]] = new Pointer[Pointer[u8]](
      environ,
      (i) => new Pointer[u8](i, (i) => mem.get(i), (i, r) => mem.put(i, `r`)),
      (i, r) => mem.putInt(i, `r`.offset))
    val environ_bufAdapted: Pointer[u8] = new Pointer[u8](environ_buf, (i) => mem.get(i), (i, r) => mem.put(i, `r`))

    IO(environ_getImpl(environAdapted, environ_bufAdapted).id)
  }

  def environ_sizes_getImpl(environc: Pointer[size], environ_buf_size: Pointer[size]): (errnoEnum.Value)

  def environ_sizes_get(environc: Int, environ_buf_size: Int) = {
    val environcAdapted: Pointer[size] = new Pointer[size](environc, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))
    val environ_buf_sizeAdapted: Pointer[size] =
      new Pointer[size](environ_buf_size, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(environ_sizes_getImpl(environcAdapted, environ_buf_sizeAdapted).id)
  }

  def clock_res_getImpl(id: clockidEnum.Value, resolution: Pointer[timestamp]): (errnoEnum.Value)

  def clock_res_get(id: Int, resolution: Int) = {
    val idAdapted: clockidEnum.Value = clockidEnum(id)
    val resolutionAdapted: Pointer[timestamp] =
      new Pointer[timestamp](resolution, (i) => mem.getLong(i), (i, r) => mem.putLong(i, `r`))

    IO(clock_res_getImpl(idAdapted, resolutionAdapted).id)
  }

  def clock_time_getImpl(id: clockidEnum.Value, precision: timestamp, time: Pointer[timestamp]): (errnoEnum.Value)

  def clock_time_get(id: Int, precision: Long, time: Int) = {
    val idAdapted: clockidEnum.Value = clockidEnum(id)
    val timeAdapted: Pointer[timestamp] =
      new Pointer[timestamp](time, (i) => mem.getLong(i), (i, r) => mem.putLong(i, `r`))

    IO(clock_time_getImpl(idAdapted, precision, timeAdapted).id)
  }

  def fd_adviseImpl(fd: fd, offset: filesize, len: filesize, advice: adviceEnum.Value): (errnoEnum.Value)

  def fd_advise(fd: Int, offset: Long, len: Long, advice: Int) = {
    val adviceAdapted: adviceEnum.Value = adviceEnum(advice)

    IO(fd_adviseImpl(fd, offset, len, adviceAdapted).id)
  }

  def fd_allocateImpl(fd: fd, offset: filesize, len: filesize): (errnoEnum.Value)

  def fd_allocate(fd: Int, offset: Long, len: Long) = {

    IO(fd_allocateImpl(fd, offset, len).id)
  }

  def fd_closeImpl(fd: fd): (errnoEnum.Value)

  def fd_close(fd: Int) = {

    IO(fd_closeImpl(fd).id)
  }

  def fd_datasyncImpl(fd: fd): (errnoEnum.Value)

  def fd_datasync(fd: Int) = {

    IO(fd_datasyncImpl(fd).id)
  }

  def fd_fdstat_getImpl(fd: fd, stat: fdstat, offset: Int): (errnoEnum.Value)

  def fd_fdstat_get(fd: Int, stat: Int) = {
    val statAdapted: fdstat = fdstat(-1, -1, -1, -1)

    IO(fd_fdstat_getImpl(fd, statAdapted, stat).id)
  }

  def fd_fdstat_set_flagsImpl(fd: fd, flags: fdflagsFlags.Value): (errnoEnum.Value)

  def fd_fdstat_set_flags(fd: Int, flags: Int) = {
    val flagsAdapted: fdflagsFlags.Value = fdflagsFlags(flags)

    IO(fd_fdstat_set_flagsImpl(fd, flagsAdapted).id)
  }

  def fd_fdstat_set_rightsImpl(fd: fd,
                               fs_rights_base: rightsFlags.Value,
                               fs_rights_inheriting: rightsFlags.Value): (errnoEnum.Value)

  def fd_fdstat_set_rights(fd: Int, fs_rights_base: Int, fs_rights_inheriting: Int) = {
    val fs_rights_baseAdapted: rightsFlags.Value = rightsFlags(fs_rights_base)
    val fs_rights_inheritingAdapted: rightsFlags.Value = rightsFlags(fs_rights_inheriting)

    IO(fd_fdstat_set_rightsImpl(fd, fs_rights_baseAdapted, fs_rights_inheritingAdapted).id)
  }

  def fd_filestat_getImpl(fd: fd, buf: Pointer[filestat]): (errnoEnum.Value)

  def fd_filestat_get(fd: Int, buf: Int) = {
    val bufAdapted: Pointer[filestat] = new Pointer[filestat](buf, (i) => filestat(mem, i), (i, r) => r.write(i, mem))

    IO(fd_filestat_getImpl(fd, bufAdapted).id)
  }

  def fd_filestat_set_sizeImpl(fd: fd, size: filesize): (errnoEnum.Value)

  def fd_filestat_set_size(fd: Int, size: Long) = {

    IO(fd_filestat_set_sizeImpl(fd, size).id)
  }

  def fd_filestat_set_timesImpl(fd: fd,
                                atim: timestamp,
                                mtim: timestamp,
                                fst_flags: fstflagsFlags.Value): (errnoEnum.Value)

  def fd_filestat_set_times(fd: Int, atim: Long, mtim: Long, fst_flags: Int) = {
    val fst_flagsAdapted: fstflagsFlags.Value = fstflagsFlags(fst_flags)

    IO(fd_filestat_set_timesImpl(fd, atim, mtim, fst_flagsAdapted).id)
  }

  def fd_preadImpl(fd: fd, iovs: iovec_array, iovsLen: u32, offset: filesize, nread: Pointer[size]): (errnoEnum.Value)

  def fd_pread(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nread: Int) = {
    val iovsAdapted: iovec_array = new ArrayInstance[iovec](iovs, iovsLen, 8, (i) => `iovec`(mem, i)).values
    val nreadAdapted: Pointer[size] = new Pointer[size](nread, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(fd_preadImpl(fd, iovsAdapted, iovsLen, offset, nreadAdapted).id)
  }

  def fd_prestat_getImpl(fd: fd, buf: Pointer[prestat]): (errnoEnum.Value)

  def fd_prestat_get(fd: Int, buf: Int) = {
    val bufAdapted: Pointer[prestat] = new Pointer[prestat](buf, (i) => prestat(mem, i), (i, r) => `r`.write(i, mem))

    IO(fd_prestat_getImpl(fd, bufAdapted).id)
  }

  def fd_prestat_dir_nameImpl(fd: fd, path: Pointer[u8], path_len: size): (errnoEnum.Value)

  def fd_prestat_dir_name(fd: Int, path: Int, path_len: Int) = {
    val pathAdapted: Pointer[u8] = new Pointer[u8](path, (i) => mem.get(i), (i, r) => mem.put(i, `r`))

    IO(fd_prestat_dir_nameImpl(fd, pathAdapted, path_len).id)
  }

  def fd_pwriteImpl(fd: fd,
                    iovs: ciovec_array,
                    iovsLen: u32,
                    offset: filesize,
                    nwritten: Pointer[size]): (errnoEnum.Value)

  def fd_pwrite(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nwritten: Int) = {
    val iovsAdapted: ciovec_array = new ArrayInstance[ciovec](iovs, iovsLen, 8, (i) => `ciovec`(mem, i)).values
    val nwrittenAdapted: Pointer[size] = new Pointer[size](nwritten, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(fd_pwriteImpl(fd, iovsAdapted, iovsLen, offset, nwrittenAdapted).id)
  }

  def fd_readImpl(fd: fd, iovs: iovec_array, iovsLen: u32, nread: Pointer[size]): (errnoEnum.Value)

  def fd_read(fd: Int, iovs: Int, iovsLen: Int, nread: Int) = {
    val iovsAdapted: iovec_array = new ArrayInstance[iovec](iovs, iovsLen, 8, (i) => iovec(mem, i)).values
    val nreadAdapted: Pointer[size] = new Pointer[size](nread, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(fd_readImpl(fd, iovsAdapted, iovsLen, nreadAdapted).id)
  }

  def fd_readdirImpl(fd: fd,
                     buf: Pointer[u8],
                     buf_len: size,
                     cookie: dircookie,
                     bufused: Pointer[size]): (errnoEnum.Value)

  def fd_readdir(fd: Int, buf: Int, buf_len: Int, cookie: Long, bufused: Int) = {
    val bufAdapted: Pointer[u8] = new Pointer[u8](buf, (i) => mem.get(i), (i, r) => mem.put(i, `r`))
    val bufusedAdapted: Pointer[size] = new Pointer[size](bufused, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(fd_readdirImpl(fd, bufAdapted, buf_len, cookie, bufusedAdapted).id)
  }

  def fd_renumberImpl(fd: fd, to: fd): (errnoEnum.Value)

  def fd_renumber(fd: Int, to: Int) = {

    IO(fd_renumberImpl(fd, to).id)
  }

  def fd_seekImpl(fd: fd, offset: filedelta, whence: whenceEnum.Value, newoffset: Pointer[filesize]): (errnoEnum.Value)

  def fd_seek(fd: Int, offset: Long, whence: Int, newoffset: Int) = {
    val whenceAdapted: whenceEnum.Value = whenceEnum(whence)
    val newoffsetAdapted: Pointer[filesize] =
      new Pointer[filesize](newoffset, (i) => mem.getLong(i), (i, r) => mem.putLong(i, `r`))

    IO(fd_seekImpl(fd, offset, whenceAdapted, newoffsetAdapted).id)
  }

  def fd_syncImpl(fd: fd): (errnoEnum.Value)

  def fd_sync(fd: Int) = {

    IO(fd_syncImpl(fd).id)
  }

  def fd_tellImpl(fd: fd, offset: Pointer[filesize]): (errnoEnum.Value)

  def fd_tell(fd: Int, offset: Int) = {
    val offsetAdapted: Pointer[filesize] =
      new Pointer[filesize](offset, (i) => mem.getLong(i), (i, r) => mem.putLong(i, `r`))

    IO(fd_tellImpl(fd, offsetAdapted).id)
  }

  def fd_writeImpl(fd: fd, iovs: ciovec_array, iovsLen: u32, nwritten: Pointer[size]): (errnoEnum.Value)

  def fd_write(fd: Int, iovs: Int, iovsLen: Int, nwritten: Int) = {
    val iovsAdapted: ciovec_array = new ArrayInstance[ciovec](iovs, iovsLen, 8, (i) => `ciovec`(mem, i)).values

    val nwrittenAdapted: Pointer[size] = new Pointer[size](nwritten, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(fd_writeImpl(fd, iovsAdapted, iovsLen, nwrittenAdapted).id)
  }

  def path_create_directoryImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_create_directory(fd: Int, path: Int, pathLen: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)

    IO(path_create_directoryImpl(fd, pathAdapted).id)
  }

  def path_filestat_getImpl(fd: fd,
                            flags: lookupflagsFlags.Value,
                            path: string,
                            buf: Pointer[filestat]): (errnoEnum.Value)

  def path_filestat_get(fd: Int, flags: Int, path: Int, pathLen: Int, buf: Int) = {
    val flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(flags)
    val pathAdapted: String = getString(mem, path, pathLen)
    val bufAdapted: Pointer[filestat] = new Pointer[filestat](buf, (i) => filestat(mem, i), (i, r) => r.write(i, mem))

    IO(path_filestat_getImpl(fd, flagsAdapted, pathAdapted, bufAdapted).id)
  }

  def path_filestat_set_timesImpl(fd: fd,
                                  flags: lookupflagsFlags.Value,
                                  path: string,
                                  atim: timestamp,
                                  mtim: timestamp,
                                  fst_flags: fstflagsFlags.Value): (errnoEnum.Value)

  def path_filestat_set_times(fd: Int, flags: Int, path: Int, pathLen: Int, atim: Long, mtim: Long, fst_flags: Int) = {
    val flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(flags)
    val pathAdapted: String = getString(mem, path, pathLen)
    val fst_flagsAdapted: fstflagsFlags.Value = fstflagsFlags(fst_flags)

    IO(path_filestat_set_timesImpl(fd, flagsAdapted, pathAdapted, atim, mtim, fst_flagsAdapted).id)
  }

  def path_linkImpl(old_fd: fd,
                    old_flags: lookupflagsFlags.Value,
                    old_path: string,
                    new_fd: fd,
                    new_path: string): (errnoEnum.Value)

  def path_link(old_fd: Int, old_flags: Int, old_path: Int, oldLen: Int, new_fd: Int, new_path: Int, newLen: Int) = {
    val old_flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(old_flags)
    val old_pathAdapted: String = getString(mem, old_path, oldLen)
    val new_pathAdapted: String = getString(mem, new_path, newLen)

    IO(path_linkImpl(old_fd, old_flagsAdapted, old_pathAdapted, new_fd, new_pathAdapted).id)
  }

  def path_openImpl(fd: fd,
                    dirflags: lookupflagsFlags.Value,
                    path: string,
                    pathLen: Int,
                    oflags: oflagsFlags.Value,
                    fs_rights_base: rightsFlags.Value,
                    fs_rights_inherting: rightsFlags.Value,
                    fdflags: fdflagsFlags.Value,
                    opened_fd: Pointer[fd]): (errnoEnum.Value)

  def path_open(fd: Int,
                dirflags: Int,
                path: Int,
                pathLen: Int,
                oflags: Int,
                fs_rights_base: Long,
                fs_rights_inherting: Long,
                fdflags: Int,
                opened_fd: Int) = {
    val dirflagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(dirflags)
    val pathAdapted: String = getString(mem, path, pathLen)
    val oflagsAdapted: oflagsFlags.Value = oflagsFlags(oflags)
    val fs_rights_baseAdapted: rightsFlags.Value = rightsFlags(fs_rights_base.toInt)
    val fs_rights_inhertingAdapted: rightsFlags.Value = rightsFlags(fs_rights_inherting.toInt)
    val fdflagsAdapted: fdflagsFlags.Value = fdflagsFlags(fdflags)
    val opened_fdAdapted: Pointer[fd] = new Pointer[fd](opened_fd, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(
      path_openImpl(fd,
                    dirflagsAdapted,
                    pathAdapted,
                    pathLen,
                    oflagsAdapted,
                    fs_rights_baseAdapted,
                    fs_rights_inhertingAdapted,
                    fdflagsAdapted,
                    opened_fdAdapted).id)
  }

  def path_readlinkImpl(fd: fd,
                        path: string,
                        buf: Pointer[u8],
                        buf_len: size,
                        bufused: Pointer[size]): (errnoEnum.Value)

  def path_readlink(fd: Int, path: Int, pathLen: Int, buf: Int, buf_len: Int, bufused: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)
    val bufAdapted: Pointer[u8] = new Pointer[u8](buf, (i) => mem.get(i), (i, r) => mem.put(i, `r`))
    val bufusedAdapted: Pointer[size] = new Pointer[size](bufused, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(path_readlinkImpl(fd, pathAdapted, bufAdapted, buf_len, bufusedAdapted).id)
  }

  def path_remove_directoryImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_remove_directory(fd: Int, path: Int, pathLen: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)

    IO(path_remove_directoryImpl(fd, pathAdapted).id)
  }

  def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): (errnoEnum.Value)

  def path_rename(fd: Int, old_path: Int, oldLen: Int, new_fd: Int, new_path: Int, newLen: Int) = {
    val old_pathAdapted: String = getString(mem, old_path, oldLen)
    val new_pathAdapted: String = getString(mem, new_path, newLen)

    IO(path_renameImpl(fd, old_pathAdapted, new_fd, new_pathAdapted).id)
  }

  def path_symlinkImpl(old_path: string, fd: fd, new_path: string): (errnoEnum.Value)

  def path_symlink(old_path: Int, oldLen: Int, fd: Int, new_path: Int, newLen: Int) = {
    val old_pathAdapted: String = getString(mem, old_path, oldLen)
    val new_pathAdapted: String = getString(mem, new_path, newLen)

    IO(path_symlinkImpl(old_pathAdapted, fd, new_pathAdapted).id)
  }

  def path_unlink_fileImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_unlink_file(fd: Int, path: Int, pathLen: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)

    IO(path_unlink_fileImpl(fd, pathAdapted).id)
  }

  def poll_oneoffImpl(in: Pointer[subscription],
                      out: Pointer[event],
                      nsubscriptions: size,
                      nevents: Pointer[size]): (errnoEnum.Value)

  def poll_oneoff(in: Int, out: Int, nsubscriptions: Int, nevents: Int) = {
    val inAdapted: Pointer[subscription] =
      new Pointer[subscription](in, (i) => subscription(mem, i), (i, r) => r.write(i, mem))
    val outAdapted: Pointer[event] = new Pointer[event](out, (i) => event(mem, i), (i, r) => r.write(i, mem))
    val neventsAdapted: Pointer[size] = new Pointer[size](nevents, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(poll_oneoffImpl(inAdapted, outAdapted, nsubscriptions, neventsAdapted).id)
  }

  def proc_exitImpl(rval: exitcode)

  def proc_exit(rval: Int) = {

    IO(proc_exitImpl(rval))
  }

  def proc_raiseImpl(sig: signalEnum.Value): (errnoEnum.Value)

  def proc_raise(sig: Int) = {
    val sigAdapted: signalEnum.Value = signalEnum(sig)

    IO(proc_raiseImpl(sigAdapted).id)
  }

  def sched_yieldImpl(): (errnoEnum.Value)

  def sched_yield() = {

    IO(sched_yieldImpl().id)
  }

  def random_getImpl(buf: Pointer[u8], buf_len: size): (errnoEnum.Value)

  def random_get(buf: Int, buf_len: Int) = {
    val bufAdapted: Pointer[u8] = new Pointer[u8](mem.getInt(buf), (i) => mem.get(i), (i, r) => mem.put(i, `r`))

    IO(random_getImpl(bufAdapted, buf_len).id)
  }

  def sock_recvImpl(fd: fd,
                    ri_data: iovec_array,
                    ri_dataLen: u32,
                    ri_flags: riflagsFlags.Value,
                    ro_datalen: Pointer[size],
                    ro_flags: Pointer[roflagsFlags.Value]): (errnoEnum.Value)

  def sock_recv(fd: Int, ri_data: Int, ri_dataLen: Int, ri_flags: Int, ro_datalen: Int, ro_flags: Int) = {
    val ri_dataAdapted: iovec_array = new ArrayInstance[iovec](ri_data, ri_dataLen, 8, (i) => `iovec`(mem, i)).values
    val ri_flagsAdapted: riflagsFlags.Value = riflagsFlags(ri_flags)
    val ro_datalenAdapted: Pointer[size] =
      new Pointer[size](ro_datalen, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))
    val ro_flagsAdapted: Pointer[roflagsFlags.Value] = new Pointer[roflagsFlags.Value](
      ro_flags,
      (i) => roflagsFlags(mem.getShort(i)),
      (i, r) => mem.putShort(i, `r`.id.toShort))

    IO(sock_recvImpl(fd, ri_dataAdapted, ri_dataLen, ri_flagsAdapted, ro_datalenAdapted, ro_flagsAdapted).id)
  }

  def sock_sendImpl(fd: fd,
                    si_data: ciovec_array,
                    si_dataLen: u32,
                    si_flags: siflags,
                    so_datalen: Pointer[size]): (errnoEnum.Value)

  def sock_send(fd: Int, si_data: Int, si_dataLen: Int, si_flags: Int, so_datalen: Int) = {
    val si_dataAdapted: ciovec_array = new ArrayInstance[ciovec](si_data, si_dataLen, 8, (i) => `ciovec`(mem, i)).values
    val si_flagsAdapted: Short = si_flags.toShort
    val so_datalenAdapted: Pointer[size] =
      new Pointer[size](so_datalen, (i) => mem.getInt(i), (i, r) => mem.putInt(i, `r`))

    IO(sock_sendImpl(fd, si_dataAdapted, si_dataLen, si_flagsAdapted, so_datalenAdapted).id)
  }

  def sock_shutdownImpl(fd: fd, how: sdflagsFlags.Value): (errnoEnum.Value)

  def sock_shutdown(fd: Int, how: Int) = {
    val howAdapted: sdflagsFlags.Value = sdflagsFlags(how)

    IO(sock_shutdownImpl(fd, howAdapted).id)
  }

}
