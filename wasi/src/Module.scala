package swam
package wasi

import Types._
import Header._
import cats.Applicative
import cats.effect._
import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._
import cats.effect.IO
import swam.runtime.Memory
import swam.runtime.imports.annotations.{effect, effectful, module, pure}

@module
abstract class Module[@effect F[_]](implicit F: Applicative[F]) {
  var mem: Memory[IO] = null

  val name = "wasi_snapshot_preview1"

  def tryToExecute(a: => errnoEnum.Value) = {
    try a.id
    catch {
      case x: WASIException => x.errno.id
    }
  }

  def args_getImpl(argv: ptr, argv_buf: ptr): errnoEnum.Value

  @effectful
  def args_get(argv: Int, argv_buf: Int): F[Int] =
    F.pure({
      tryToExecute(args_getImpl(argv, argv_buf))
    })

  def args_sizes_getImpl(argc: ptr, argv_buf_size: ptr): errnoEnum.Value

  @effectful
  def args_sizes_get(argc: Int, argv_buf_size: Int): F[Int] =
    F.pure({
      tryToExecute(args_sizes_getImpl(argc, argv_buf_size))
    })

  def environ_getImpl(environ: ptr, environ_buf: ptr): errnoEnum.Value

  @effectful
  def environ_get(environ: Int, environ_buf: Int): F[Int] =
    F.pure({
      tryToExecute(environ_getImpl(environ, environ_buf))
    })

  def environ_sizes_getImpl(environc: ptr, environ_buf_size: ptr): errnoEnum.Value

  @effectful
  def environ_sizes_get(environc: Int, environ_buf_size: Int): F[Int] =
    F.pure({
      tryToExecute(environ_sizes_getImpl(environc, environ_buf_size))
    })

  def clock_res_getImpl(id: clockidEnum.Value, resolution: ptr): errnoEnum.Value

  @effectful
  def clock_res_get(id: Int, resolution: Int): F[Int] =
    F.pure({
      val idAdapted: clockidEnum.Value = clockidEnum(id)
      tryToExecute(clock_res_getImpl(idAdapted, resolution))
    })

  def clock_time_getImpl(id: clockidEnum.Value, precision: timestamp, time: ptr): errnoEnum.Value

  @effectful
  def clock_time_get(id: Int, precision: Long, time: Int): F[Int] =
    F.pure({
      val idAdapted: clockidEnum.Value = clockidEnum(id)
      tryToExecute(clock_time_getImpl(idAdapted, precision, time))
    })

  def fd_adviseImpl(fd: fd, offset: filesize, len: filesize, advice: adviceEnum.Value): errnoEnum.Value

  @effectful
  def fd_advise(fd: Int, offset: Long, len: Long, advice: Int): F[Int] =
    F.pure({
      val adviceAdapted: adviceEnum.Value = adviceEnum(advice)
      tryToExecute(fd_adviseImpl(fd, offset, len, adviceAdapted))
    })

  def fd_allocateImpl(fd: fd, offset: filesize, len: filesize): errnoEnum.Value

  @effectful
  def fd_allocate(fd: Int, offset: Long, len: Long): F[Int] =
    F.pure({
      tryToExecute(fd_allocateImpl(fd, offset, len))
    })

  def fd_closeImpl(fd: fd): errnoEnum.Value

  @effectful
  def fd_close(fd: Int): F[Int] =
    F.pure({
      tryToExecute(fd_closeImpl(fd))
    })

  def fd_datasyncImpl(fd: fd): errnoEnum.Value

  @effectful
  def fd_datasync(fd: Int): F[Int] =
    F.pure({
      tryToExecute(fd_datasyncImpl(fd))
    })

  def fd_fdstat_getImpl(fd: fd, stat: ptr): errnoEnum.Value

  @effectful
  def fd_fdstat_get(fd: Int, stat: Int): F[Int] =
    F.pure({
      tryToExecute(fd_fdstat_getImpl(fd, stat))
    })

  def fd_fdstat_set_flagsImpl(fd: fd, flags: fdflagsFlags.Value): errnoEnum.Value

  @effectful
  def fd_fdstat_set_flags(fd: Int, flags: Int): F[Int] =
    F.pure({
      val flagsAdapted: fdflagsFlags.Value = fdflagsFlags(flags)
      tryToExecute(fd_fdstat_set_flagsImpl(fd, flagsAdapted))
    })

  def fd_fdstat_set_rightsImpl(fd: fd,
                               fs_rights_base: rightsFlags.Value,
                               fs_rights_inheriting: rightsFlags.Value): errnoEnum.Value

  @effectful
  def fd_fdstat_set_rights(fd: Int, fs_rights_base: Int, fs_rights_inheriting: Int): F[Int] =
    F.pure({
      val fs_rights_baseAdapted: rightsFlags.Value = rightsFlags(fs_rights_base)
      val fs_rights_inheritingAdapted: rightsFlags.Value = rightsFlags(fs_rights_inheriting)
      tryToExecute(fd_fdstat_set_rightsImpl(fd, fs_rights_baseAdapted, fs_rights_inheritingAdapted))
    })

  def fd_filestat_getImpl(fd: fd, buf: ptr): errnoEnum.Value

  @effectful
  def fd_filestat_get(fd: Int, buf: Int): F[Int] =
    F.pure({
      tryToExecute(fd_filestat_getImpl(fd, buf))
    })

  def fd_filestat_set_sizeImpl(fd: fd, size: filesize): errnoEnum.Value

  @effectful
  def fd_filestat_set_size(fd: Int, size: Long): F[Int] =
    F.pure({
      tryToExecute(fd_filestat_set_sizeImpl(fd, size))
    })

  def fd_filestat_set_timesImpl(fd: fd,
                                atim: timestamp,
                                mtim: timestamp,
                                fst_flags: fstflagsFlags.Value): errnoEnum.Value

  @effectful
  def fd_filestat_set_times(fd: Int, atim: Long, mtim: Long, fst_flags: Int): F[Int] =
    F.pure({
      val fst_flagsAdapted: fstflagsFlags.Value = fstflagsFlags(fst_flags)
      tryToExecute(fd_filestat_set_timesImpl(fd, atim, mtim, fst_flagsAdapted))
    })

  def fd_preadImpl(fd: fd, iovs: iovec_array, iovsLen: u32, offset: filesize, nread: ptr): errnoEnum.Value

  @effectful
  def fd_pread(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nread: Int): F[Int] =
    F.pure({
      val iovsAdapted: iovec_array = new ArrayInstance[iovec](iovs, iovsLen, 8, (i) => iovec(mem, i)).values
      tryToExecute(fd_preadImpl(fd, iovsAdapted, iovsLen, offset, nread))
    })

  def fd_prestat_getImpl(fd: fd, buf: ptr): errnoEnum.Value

  @effectful
  def fd_prestat_get(fd: Int, buf: Int): F[Int] =
    F.pure({
      tryToExecute(fd_prestat_getImpl(fd, buf))
    })

  def fd_prestat_dir_nameImpl(fd: fd, path: ptr, path_len: size): errnoEnum.Value

  @effectful
  def fd_prestat_dir_name(fd: Int, path: Int, path_len: Int): F[Int] =
    F.pure({
      tryToExecute(fd_prestat_dir_nameImpl(fd, path, path_len))
    })

  def fd_pwriteImpl(fd: fd, iovs: ciovec_array, iovsLen: u32, offset: filesize, nwritten: ptr): errnoEnum.Value

  @effectful
  def fd_pwrite(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nwritten: Int): F[Int] =
    F.pure({
      val iovsAdapted: ciovec_array = new ArrayInstance[ciovec](iovs, iovsLen, 8, (i) => ciovec(mem, i)).values
      tryToExecute(fd_pwriteImpl(fd, iovsAdapted, iovsLen, offset, nwritten))
    })

  def fd_readImpl(fd: fd, iovs: iovec_array, iovsLen: u32, nread: ptr): errnoEnum.Value

  @effectful
  def fd_read(fd: Int, iovs: Int, iovsLen: Int, nread: Int): F[Int] =
    F.pure({
      val iovsAdapted: iovec_array = new ArrayInstance[iovec](iovs, iovsLen, 8, (i) => iovec(mem, i)).values
      tryToExecute(fd_readImpl(fd, iovsAdapted, iovsLen, nread))
    })

  def fd_readdirImpl(fd: fd, buf: ptr, buf_len: size, cookie: dircookie, bufused: ptr): errnoEnum.Value

  @effectful
  def fd_readdir(fd: Int, buf: Int, buf_len: Int, cookie: Long, bufused: Int): F[Int] =
    F.pure({
      tryToExecute(fd_readdirImpl(fd, buf, buf_len, cookie, bufused))
    })

  def fd_renumberImpl(fd: fd, to: fd): errnoEnum.Value

  @effectful
  def fd_renumber(fd: Int, to: Int): F[Int] =
    F.pure({
      tryToExecute(fd_renumberImpl(fd, to))
    })

  def fd_seekImpl(fd: fd, offset: filedelta, whence: whenceEnum.Value, newoffset: ptr): errnoEnum.Value

  @effectful
  def fd_seek(fd: Int, offset: Long, whence: Int, newoffset: Int): F[Int] =
    F.pure({
      val whenceAdapted: whenceEnum.Value = whenceEnum(whence)
      tryToExecute(fd_seekImpl(fd, offset, whenceAdapted, newoffset))
    })

  def fd_syncImpl(fd: fd): errnoEnum.Value

  @effectful
  def fd_sync(fd: Int): F[Int] =
    F.pure({
      tryToExecute(fd_syncImpl(fd))
    })

  def fd_tellImpl(fd: fd, offset: ptr): errnoEnum.Value

  @effectful
  def fd_tell(fd: Int, offset: Int): F[Int] =
    F.pure({
      tryToExecute(fd_tellImpl(fd, offset))
    })

  def fd_writeImpl(fd: fd, iovs: ciovec_array, iovsLen: u32, nwritten: ptr): errnoEnum.Value

  @effectful
  def fd_write(fd: Int, iovs: Int, iovsLen: Int, nwritten: Int): F[Int] =
    F.pure({
      val iovsAdapted: ciovec_array = new ArrayInstance[ciovec](iovs, iovsLen, 8, (i) => ciovec(mem, i)).values
      tryToExecute(fd_writeImpl(fd, iovsAdapted, iovsLen, nwritten))
    })

  def path_create_directoryImpl(fd: fd, path: string): errnoEnum.Value

  @effectful
  def path_create_directory(fd: Int, path: Int, pathLen: Int): F[Int] =
    F.pure({
      val pathAdapted: String = getString(mem, path, pathLen)
      tryToExecute(path_create_directoryImpl(fd, pathAdapted))
    })

  def path_filestat_getImpl(fd: fd, flags: lookupflagsFlags.Value, path: string, buf: ptr): errnoEnum.Value

  @effectful
  def path_filestat_get(fd: Int, flags: Int, path: Int, pathLen: Int, buf: Int): F[Int] =
    F.pure({
      val flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(flags)
      val pathAdapted: String = getString(mem, path, pathLen)
      tryToExecute(path_filestat_getImpl(fd, flagsAdapted, pathAdapted, buf))
    })

  def path_filestat_set_timesImpl(fd: fd,
                                  flags: lookupflagsFlags.Value,
                                  path: string,
                                  atim: timestamp,
                                  mtim: timestamp,
                                  fst_flags: fstflagsFlags.Value): errnoEnum.Value

  @effectful
  def path_filestat_set_times(fd: Int,
                              flags: Int,
                              path: Int,
                              pathLen: Int,
                              atim: Long,
                              mtim: Long,
                              fst_flags: Int): F[Int] =
    F.pure({
      val flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(flags)
      val pathAdapted: String = getString(mem, path, pathLen)
      val fst_flagsAdapted: fstflagsFlags.Value = fstflagsFlags(fst_flags)
      tryToExecute(path_filestat_set_timesImpl(fd, flagsAdapted, pathAdapted, atim, mtim, fst_flagsAdapted))
    })

  def path_linkImpl(old_fd: fd,
                    old_flags: lookupflagsFlags.Value,
                    old_path: string,
                    new_fd: fd,
                    new_path: string): errnoEnum.Value

  @effectful
  def path_link(old_fd: Int,
                old_flags: Int,
                old_path: Int,
                old_pathLen: Int,
                new_fd: Int,
                new_path: Int,
                new_pathLen: Int): F[Int] =
    F.pure({
      val old_flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(old_flags)
      val old_pathAdapted: String = getString(mem, old_path, old_pathLen)
      val new_pathAdapted: String = getString(mem, new_path, new_pathLen)
      tryToExecute(path_linkImpl(old_fd, old_flagsAdapted, old_pathAdapted, new_fd, new_pathAdapted))
    })

  def path_openImpl(fd: fd,
                    dirflags: Int,
                    path: ptr,
                    pathLen: Int,
                    oflags: Int,
                    fs_rights_base: Long,
                    fs_rights_inherting: Long,
                    fdflags: Int,
                    opened_fd: ptr): errnoEnum.Value

  @effectful
  def path_open(fd: Int,
                dirflags: Int,
                path: Int,
                pathLen: Int,
                oflags: Int,
                fs_rights_base: Long,
                fs_rights_inherting: Long,
                fdflags: Int,
                opened_fd: Int): F[Int] =
    F.pure({
      tryToExecute(
        path_openImpl(fd, dirflags, path, pathLen, oflags, fs_rights_base, fs_rights_inherting, fdflags, opened_fd)
      )
    })

  def path_readlinkImpl(fd: fd, path: string, buf: ptr, buf_len: size, bufused: ptr): errnoEnum.Value

  @effectful
  def path_readlink(fd: Int, path: Int, pathLen: Int, buf: Int, buf_len: Int, bufused: Int): F[Int] =
    F.pure({
      val pathAdapted: String = getString(mem, path, pathLen)
      tryToExecute(path_readlinkImpl(fd, pathAdapted, buf, buf_len, bufused))
    })

  def path_remove_directoryImpl(fd: fd, path: string): errnoEnum.Value

  @effectful
  def path_remove_directory(fd: Int, path: Int, pathLen: Int): F[Int] =
    F.pure({
      val pathAdapted: String = getString(mem, path, pathLen)
      tryToExecute(path_remove_directoryImpl(fd, pathAdapted))
    })

  def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): errnoEnum.Value

  @effectful
  def path_rename(fd: Int, old_path: Int, old_pathLen: Int, new_fd: Int, new_path: Int, new_pathLen: Int): F[Int] =
    F.pure({
      val old_pathAdapted: String = getString(mem, old_path, old_pathLen)
      val new_pathAdapted: String = getString(mem, new_path, new_pathLen)
      tryToExecute(path_renameImpl(fd, old_pathAdapted, new_fd, new_pathAdapted))
    })

  def path_symlinkImpl(old_path: string, fd: fd, new_path: string): errnoEnum.Value

  @effectful
  def path_symlink(old_path: Int, old_pathLen: Int, fd: Int, new_path: Int, new_pathLen: Int): F[Int] =
    F.pure({
      val old_pathAdapted: String = getString(mem, old_path, old_pathLen)
      val new_pathAdapted: String = getString(mem, new_path, new_pathLen)
      tryToExecute(path_symlinkImpl(old_pathAdapted, fd, new_pathAdapted))
    })

  def path_unlink_fileImpl(fd: fd, path: string): errnoEnum.Value

  @effectful
  def path_unlink_file(fd: Int, path: Int, pathLen: Int): F[Int] =
    F.pure({
      val pathAdapted: String = getString(mem, path, pathLen)
      tryToExecute(path_unlink_fileImpl(fd, pathAdapted))
    })

  def poll_oneoffImpl(in: ptr, out: ptr, nsubscriptions: size, nevents: ptr): errnoEnum.Value

  @effectful
  def poll_oneoff(in: Int, out: Int, nsubscriptions: Int, nevents: Int): F[Int] =
    F.pure({
      tryToExecute(poll_oneoffImpl(in, out, nsubscriptions, nevents))
    })

  def proc_exitImpl(rval: exitcode): Unit

  @effectful
  def proc_exit(rval: Int): F[Unit] =
    F.pure({
      proc_exitImpl(rval)
    })

  def proc_raiseImpl(sig: signalEnum.Value): errnoEnum.Value

  @effectful
  def proc_raise(sig: Int): F[Int] =
    F.pure({
      val sigAdapted: signalEnum.Value = signalEnum(sig)
      tryToExecute(proc_raiseImpl(sigAdapted))
    })

  def sched_yieldImpl(): errnoEnum.Value

  @effectful
  def sched_yield(): F[Int] =
    F.pure({
      tryToExecute(sched_yieldImpl())
    })

  def random_getImpl(buf: ptr, buf_len: size): errnoEnum.Value

  @effectful
  def random_get(buf: Int, buf_len: Int): F[Int] =
    F.pure({
      tryToExecute(random_getImpl(buf, buf_len))
    })

  def sock_recvImpl(fd: fd,
                    ri_data: iovec_array,
                    ri_dataLen: u32,
                    ri_flags: riflagsFlags.Value,
                    ro_datalen: ptr,
                    ro_flags: ptr): errnoEnum.Value

  @effectful
  def sock_recv(fd: Int, ri_data: Int, ri_dataLen: Int, ri_flags: Int, ro_datalen: Int, ro_flags: Int): F[Int] =
    F.pure({
      val ri_dataAdapted: iovec_array = new ArrayInstance[iovec](ri_data, ri_dataLen, 8, (i) => iovec(mem, i)).values
      val ri_flagsAdapted: riflagsFlags.Value = riflagsFlags(ri_flags)
      tryToExecute(sock_recvImpl(fd, ri_dataAdapted, ri_dataLen, ri_flagsAdapted, ro_datalen, ro_flags))
    })

  def sock_sendImpl(fd: fd, si_data: ciovec_array, si_dataLen: u32, si_flags: siflags, so_datalen: ptr): errnoEnum.Value

  @effectful
  def sock_send(fd: Int, si_data: Int, si_dataLen: Int, si_flags: Int, so_datalen: Int): F[Int] =
    F.pure({
      val si_dataAdapted: ciovec_array = new ArrayInstance[ciovec](si_data, si_dataLen, 8, (i) => ciovec(mem, i)).values
      val si_flagsAdapted: Short = si_flags.toShort
      tryToExecute(sock_sendImpl(fd, si_dataAdapted, si_dataLen, si_flagsAdapted, so_datalen))
    })

  def sock_shutdownImpl(fd: fd, how: sdflagsFlags.Value): errnoEnum.Value

  @effectful
  def sock_shutdown(fd: Int, how: Int): F[Int] =
    F.pure({
      val howAdapted: sdflagsFlags.Value = sdflagsFlags(how)
      tryToExecute(sock_shutdownImpl(fd, howAdapted))
    })

}
