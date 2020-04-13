package swam
package wasi
import Types._
import Header._
import cats.effect._
import cats.effect.IO
import swam.runtime.Memory
trait Module {
  var mem: Memory[IO] = null

  def args_getImpl(argv: Pointer[Pointer[u8]], argv_buf: Pointer[u8]): (errnoEnum.Value)

  def args_get(argv: Int, argv_buf: Int) = {
    val argvAdapted: Pointer[Pointer[u8]] = new Pointer[Pointer[u8]](
      mem.readInt(argv).unsafeRunSync,
      (i) =>
        new Pointer[u8](mem.readInt(i).unsafeRunSync,
                        (i) => mem.readByte(i).unsafeRunSync(),
                        (i, r) => mem.writeByte(i, `r`).unsafeRunSync),
      (i, r) => mem.writeInt(i, `r`.offset).unsafeRunSync)
    val argv_bufAdapted: Pointer[u8] = new Pointer[u8](mem.readInt(argv_buf).unsafeRunSync,
                                                       (i) => mem.readByte(i).unsafeRunSync(),
                                                       (i, r) => mem.writeByte(i, `r`).unsafeRunSync)

    IO(try {
      args_getImpl(argvAdapted, argv_bufAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def args_sizes_getImpl(argc: Pointer[size], argv_buf_size: Pointer[size]): (errnoEnum.Value)

  def args_sizes_get(argc: Int, argv_buf_size: Int) = {
    val argcAdapted: Pointer[size] = new Pointer[size](mem.readInt(argc).unsafeRunSync,
                                                       (i) => mem.readInt(i).unsafeRunSync,
                                                       (i, r) => mem.writeInt(i, `r`).unsafeRunSync)
    val argv_buf_sizeAdapted: Pointer[size] = new Pointer[size](mem.readInt(argv_buf_size).unsafeRunSync,
                                                                (i) => mem.readInt(i).unsafeRunSync,
                                                                (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      args_sizes_getImpl(argcAdapted, argv_buf_sizeAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def environ_getImpl(environ: Pointer[Pointer[u8]], environ_buf: Pointer[u8]): (errnoEnum.Value)

  def environ_get(environ: Int, environ_buf: Int) = {
    val environAdapted: Pointer[Pointer[u8]] = new Pointer[Pointer[u8]](
      mem.readInt(environ).unsafeRunSync,
      (i) =>
        new Pointer[u8](mem.readInt(i).unsafeRunSync,
                        (i) => mem.readByte(i).unsafeRunSync(),
                        (i, r) => mem.writeByte(i, `r`).unsafeRunSync),
      (i, r) => mem.writeInt(i, `r`.offset).unsafeRunSync)
    val environ_bufAdapted: Pointer[u8] = new Pointer[u8](mem.readInt(environ_buf).unsafeRunSync,
                                                          (i) => mem.readByte(i).unsafeRunSync(),
                                                          (i, r) => mem.writeByte(i, `r`).unsafeRunSync)

    IO(try {
      environ_getImpl(environAdapted, environ_bufAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def environ_sizes_getImpl(environc: Pointer[size], environ_buf_size: Pointer[size]): (errnoEnum.Value)

  def environ_sizes_get(environc: Int, environ_buf_size: Int) = {
    val environcAdapted: Pointer[size] = new Pointer[size](mem.readInt(environc).unsafeRunSync,
                                                           (i) => mem.readInt(i).unsafeRunSync,
                                                           (i, r) => mem.writeInt(i, `r`).unsafeRunSync)
    val environ_buf_sizeAdapted: Pointer[size] = new Pointer[size](mem.readInt(environ_buf_size).unsafeRunSync,
                                                                   (i) => mem.readInt(i).unsafeRunSync,
                                                                   (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      environ_sizes_getImpl(environcAdapted, environ_buf_sizeAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def clock_res_getImpl(id: clockidEnum.Value, resolution: Pointer[timestamp]): (errnoEnum.Value)

  def clock_res_get(id: Int, resolution: Int) = {
    val idAdapted: clockidEnum.Value = clockidEnum(id)
    val resolutionAdapted: Pointer[timestamp] = new Pointer[timestamp](mem.readInt(resolution).unsafeRunSync,
                                                                       (i) => mem.readLong(i).unsafeRunSync,
                                                                       (i, r) => mem.writeLong(i, `r`).unsafeRunSync)

    IO(try {
      clock_res_getImpl(idAdapted, resolutionAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def clock_time_getImpl(id: clockidEnum.Value, precision: timestamp, time: Pointer[timestamp]): (errnoEnum.Value)

  def clock_time_get(id: Int, precision: Long, time: Int) = {
    val idAdapted: clockidEnum.Value = clockidEnum(id)
    val timeAdapted: Pointer[timestamp] = new Pointer[timestamp](mem.readInt(time).unsafeRunSync,
                                                                 (i) => mem.readLong(i).unsafeRunSync,
                                                                 (i, r) => mem.writeLong(i, `r`).unsafeRunSync)

    IO(try {
      clock_time_getImpl(idAdapted, precision, timeAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_adviseImpl(fd: fd, offset: filesize, len: filesize, advice: adviceEnum.Value): (errnoEnum.Value)

  def fd_advise(fd: Int, offset: Long, len: Long, advice: Int) = {
    val adviceAdapted: adviceEnum.Value = adviceEnum(advice)

    IO(try {
      fd_adviseImpl(fd, offset, len, adviceAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_allocateImpl(fd: fd, offset: filesize, len: filesize): (errnoEnum.Value)

  def fd_allocate(fd: Int, offset: Long, len: Long) = {

    IO(try {
      fd_allocateImpl(fd, offset, len).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_closeImpl(fd: fd): (errnoEnum.Value)

  def fd_close(fd: Int) = {

    IO(try {
      fd_closeImpl(fd).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_datasyncImpl(fd: fd): (errnoEnum.Value)

  def fd_datasync(fd: Int) = {

    IO(try {
      fd_datasyncImpl(fd).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_fdstat_getImpl(fd: fd, stat: fdstat, offset: Int): (errnoEnum.Value)

  def fd_fdstat_get(fd: Int, stat: Int) = {
    val statAdapted: fdstat = fdstat(-1, -1, -1, -1)

    IO(try {
      fd_fdstat_getImpl(fd, statAdapted, stat).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_fdstat_set_flagsImpl(fd: fd, flags: fdflagsFlags.Value): (errnoEnum.Value)

  def fd_fdstat_set_flags(fd: Int, flags: Int) = {
    val flagsAdapted: fdflagsFlags.Value = fdflagsFlags(flags)

    IO(try {
      fd_fdstat_set_flagsImpl(fd, flagsAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_fdstat_set_rightsImpl(fd: fd,
                               fs_rights_base: rightsFlags.Value,
                               fs_rights_inheriting: rightsFlags.Value): (errnoEnum.Value)

  def fd_fdstat_set_rights(fd: Int, fs_rights_base: Int, fs_rights_inheriting: Int) = {
    val fs_rights_baseAdapted: rightsFlags.Value = rightsFlags(fs_rights_base)
    val fs_rights_inheritingAdapted: rightsFlags.Value = rightsFlags(fs_rights_inheriting)

    IO(try {
      fd_fdstat_set_rightsImpl(fd, fs_rights_baseAdapted, fs_rights_inheritingAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_filestat_getImpl(fd: fd, buf: Pointer[filestat]): (errnoEnum.Value)

  def fd_filestat_get(fd: Int, buf: Int) = {
    val bufAdapted: Pointer[filestat] =
      new Pointer[filestat](mem.readInt(buf).unsafeRunSync, (i) => filestat(mem, i), (i, r) => r.write(i, mem))

    IO(try {
      fd_filestat_getImpl(fd, bufAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_filestat_set_sizeImpl(fd: fd, size: filesize): (errnoEnum.Value)

  def fd_filestat_set_size(fd: Int, size: Long) = {

    IO(try {
      fd_filestat_set_sizeImpl(fd, size).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_filestat_set_timesImpl(fd: fd,
                                atim: timestamp,
                                mtim: timestamp,
                                fst_flags: fstflagsFlags.Value): (errnoEnum.Value)

  def fd_filestat_set_times(fd: Int, atim: Long, mtim: Long, fst_flags: Int) = {
    val fst_flagsAdapted: fstflagsFlags.Value = fstflagsFlags(fst_flags)

    IO(try {
      fd_filestat_set_timesImpl(fd, atim, mtim, fst_flagsAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_preadImpl(fd: fd, iovs: iovec_array, iovsLen: u32, offset: filesize, nread: Pointer[size]): (errnoEnum.Value)

  def fd_pread(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nread: Int) = {
    val iovsAdapted: iovec_array = new ArrayInstance[iovec](iovs, iovsLen, 8, (i) => iovec(mem, i)).values
    val nreadAdapted: Pointer[size] = new Pointer[size](mem.readInt(nread).unsafeRunSync,
                                                        (i) => mem.readInt(i).unsafeRunSync,
                                                        (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      fd_preadImpl(fd, iovsAdapted, iovsLen, offset, nreadAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_prestat_getImpl(fd: fd, buf: Pointer[prestat]): (errnoEnum.Value)

  def fd_prestat_get(fd: Int, buf: Int) = {
    val bufAdapted: Pointer[prestat] =
      new Pointer[prestat](mem.readInt(buf).unsafeRunSync, (i) => prestat(mem, i), (i, r) => r.write(i, mem))

    IO(try {
      fd_prestat_getImpl(fd, bufAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_prestat_dir_nameImpl(fd: fd, path: Pointer[u8], path_len: size): (errnoEnum.Value)

  def fd_prestat_dir_name(fd: Int, path: Int, path_len: Int) = {
    val pathAdapted: Pointer[u8] = new Pointer[u8](mem.readInt(path).unsafeRunSync,
                                                   (i) => mem.readByte(i).unsafeRunSync(),
                                                   (i, r) => mem.writeByte(i, `r`).unsafeRunSync)

    IO(try {
      fd_prestat_dir_nameImpl(fd, pathAdapted, path_len).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_pwriteImpl(fd: fd,
                    iovs: ciovec_array,
                    iovsLen: u32,
                    offset: filesize,
                    nwritten: Pointer[size]): (errnoEnum.Value)

  def fd_pwrite(fd: Int, iovs: Int, iovsLen: Int, offset: Long, nwritten: Int) = {
    val iovsAdapted: ciovec_array = new ArrayInstance[ciovec](iovs, iovsLen, 8, (i) => ciovec(mem, i)).values
    val nwrittenAdapted: Pointer[size] = new Pointer[size](mem.readInt(nwritten).unsafeRunSync,
                                                           (i) => mem.readInt(i).unsafeRunSync,
                                                           (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      fd_pwriteImpl(fd, iovsAdapted, iovsLen, offset, nwrittenAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_readImpl(fd: fd, iovs: iovec_array, iovsLen: u32, nread: Pointer[size]): (errnoEnum.Value)

  def fd_read(fd: Int, iovs: Int, iovsLen: Int, nread: Int) = {
    val iovsAdapted: iovec_array = new ArrayInstance[iovec](iovs, iovsLen, 8, (i) => iovec(mem, i)).values
    val nreadAdapted: Pointer[size] = new Pointer[size](mem.readInt(nread).unsafeRunSync,
                                                        (i) => mem.readInt(i).unsafeRunSync,
                                                        (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      fd_readImpl(fd, iovsAdapted, iovsLen, nreadAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_readdirImpl(fd: fd,
                     buf: Pointer[u8],
                     buf_len: size,
                     cookie: dircookie,
                     bufused: Pointer[size]): (errnoEnum.Value)

  def fd_readdir(fd: Int, buf: Int, buf_len: Int, cookie: Long, bufused: Int) = {
    val bufAdapted: Pointer[u8] = new Pointer[u8](mem.readInt(buf).unsafeRunSync,
                                                  (i) => mem.readByte(i).unsafeRunSync(),
                                                  (i, r) => mem.writeByte(i, `r`).unsafeRunSync)
    val bufusedAdapted: Pointer[size] = new Pointer[size](mem.readInt(bufused).unsafeRunSync,
                                                          (i) => mem.readInt(i).unsafeRunSync,
                                                          (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      fd_readdirImpl(fd, bufAdapted, buf_len, cookie, bufusedAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_renumberImpl(fd: fd, to: fd): (errnoEnum.Value)

  def fd_renumber(fd: Int, to: Int) = {

    IO(try {
      fd_renumberImpl(fd, to).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_seekImpl(fd: fd, offset: filedelta, whence: whenceEnum.Value, newoffset: Pointer[filesize]): (errnoEnum.Value)

  def fd_seek(fd: Int, offset: Long, whence: Int, newoffset: Int) = {
    val whenceAdapted: whenceEnum.Value = whenceEnum(whence)
    val newoffsetAdapted: Pointer[filesize] = new Pointer[filesize](mem.readInt(newoffset).unsafeRunSync,
                                                                    (i) => mem.readLong(i).unsafeRunSync,
                                                                    (i, r) => mem.writeLong(i, `r`).unsafeRunSync)

    IO(try {
      fd_seekImpl(fd, offset, whenceAdapted, newoffsetAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_syncImpl(fd: fd): (errnoEnum.Value)

  def fd_sync(fd: Int) = {

    IO(try {
      fd_syncImpl(fd).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_tellImpl(fd: fd, offset: Pointer[filesize]): (errnoEnum.Value)

  def fd_tell(fd: Int, offset: Int) = {
    val offsetAdapted: Pointer[filesize] = new Pointer[filesize](mem.readInt(offset).unsafeRunSync,
                                                                 (i) => mem.readLong(i).unsafeRunSync,
                                                                 (i, r) => mem.writeLong(i, `r`).unsafeRunSync)

    IO(try {
      fd_tellImpl(fd, offsetAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def fd_writeImpl(fd: fd, iovs: ciovec_array, iovsLen: u32, nwritten: Int): (errnoEnum.Value)

  def fd_write(fd: Int, iovs: Int, iovsLen: Int, nwritten: Int) = {
    val iovsAdapted: ciovec_array = new ArrayInstance[ciovec](iovs, iovsLen, 8, (i) => ciovec(mem, i)).values

    IO(try {
      fd_writeImpl(fd, iovsAdapted, iovsLen, nwritten).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_create_directoryImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_create_directory(fd: Int, path: Int, pathLen: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)

    IO(try {
      path_create_directoryImpl(fd, pathAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_filestat_getImpl(fd: fd,
                            flags: lookupflagsFlags.Value,
                            path: string,
                            buf: Pointer[filestat]): (errnoEnum.Value)

  def path_filestat_get(fd: Int, flags: Int, path: Int, pathLen: Int, buf: Int) = {
    val flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(flags)
    val pathAdapted: String = getString(mem, path, pathLen)
    val bufAdapted: Pointer[filestat] =
      new Pointer[filestat](mem.readInt(buf).unsafeRunSync, (i) => filestat(mem, i), (i, r) => r.write(i, mem))

    IO(try {
      path_filestat_getImpl(fd, flagsAdapted, pathAdapted, bufAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
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

    IO(try {
      path_filestat_set_timesImpl(fd, flagsAdapted, pathAdapted, atim, mtim, fst_flagsAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_linkImpl(old_fd: fd,
                    old_flags: lookupflagsFlags.Value,
                    old_path: string,
                    new_fd: fd,
                    new_path: string): (errnoEnum.Value)

  def path_link(old_fd: Int,
                old_flags: Int,
                old_path: Int,
                old_pathLen: Int,
                new_fd: Int,
                new_path: Int,
                new_pathLen: Int) = {
    val old_flagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(old_flags)
    val old_pathAdapted: String = getString(mem, old_path, old_pathLen)
    val new_pathAdapted: String = getString(mem, new_path, new_pathLen)

    IO(try {
      path_linkImpl(old_fd, old_flagsAdapted, old_pathAdapted, new_fd, new_pathAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
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
                pathLen: Int,
                oflags: Int,
                fs_rights_base: Int,
                fs_rights_inherting: Int,
                fdflags: Int,
                opened_fd: Int) = {
    val dirflagsAdapted: lookupflagsFlags.Value = lookupflagsFlags(dirflags)
    val pathAdapted: String = getString(mem, path, pathLen)
    val oflagsAdapted: oflagsFlags.Value = oflagsFlags(oflags)
    val fs_rights_baseAdapted: rightsFlags.Value = rightsFlags(fs_rights_base)
    val fs_rights_inhertingAdapted: rightsFlags.Value = rightsFlags(fs_rights_inherting)
    val fdflagsAdapted: fdflagsFlags.Value = fdflagsFlags(fdflags)
    val opened_fdAdapted: Pointer[fd] = new Pointer[fd](mem.readInt(opened_fd).unsafeRunSync,
                                                        (i) => mem.readInt(i).unsafeRunSync,
                                                        (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      path_openImpl(fd,
                    dirflagsAdapted,
                    pathAdapted,
                    oflagsAdapted,
                    fs_rights_baseAdapted,
                    fs_rights_inhertingAdapted,
                    fdflagsAdapted,
                    opened_fdAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_readlinkImpl(fd: fd,
                        path: string,
                        buf: Pointer[u8],
                        buf_len: size,
                        bufused: Pointer[size]): (errnoEnum.Value)

  def path_readlink(fd: Int, path: Int, pathLen: Int, buf: Int, buf_len: Int, bufused: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)
    val bufAdapted: Pointer[u8] = new Pointer[u8](mem.readInt(buf).unsafeRunSync,
                                                  (i) => mem.readByte(i).unsafeRunSync(),
                                                  (i, r) => mem.writeByte(i, `r`).unsafeRunSync)
    val bufusedAdapted: Pointer[size] = new Pointer[size](mem.readInt(bufused).unsafeRunSync,
                                                          (i) => mem.readInt(i).unsafeRunSync,
                                                          (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      path_readlinkImpl(fd, pathAdapted, bufAdapted, buf_len, bufusedAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_remove_directoryImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_remove_directory(fd: Int, path: Int, pathLen: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)

    IO(try {
      path_remove_directoryImpl(fd, pathAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_renameImpl(fd: fd, old_path: string, new_fd: fd, new_path: string): (errnoEnum.Value)

  def path_rename(fd: Int, old_path: Int, old_pathLen: Int, new_fd: Int, new_path: Int, new_pathLen: Int) = {
    val old_pathAdapted: String = getString(mem, old_path, old_pathLen)
    val new_pathAdapted: String = getString(mem, new_path, new_pathLen)

    IO(try {
      path_renameImpl(fd, old_pathAdapted, new_fd, new_pathAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_symlinkImpl(old_path: string, fd: fd, new_path: string): (errnoEnum.Value)

  def path_symlink(old_path: Int, old_pathLen: Int, fd: Int, new_path: Int, new_pathLen: Int) = {
    val old_pathAdapted: String = getString(mem, old_path, old_pathLen)
    val new_pathAdapted: String = getString(mem, new_path, new_pathLen)

    IO(try {
      path_symlinkImpl(old_pathAdapted, fd, new_pathAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def path_unlink_fileImpl(fd: fd, path: string): (errnoEnum.Value)

  def path_unlink_file(fd: Int, path: Int, pathLen: Int) = {
    val pathAdapted: String = getString(mem, path, pathLen)

    IO(try {
      path_unlink_fileImpl(fd, pathAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def poll_oneoffImpl(in: Pointer[subscription],
                      out: Pointer[event],
                      nsubscriptions: size,
                      nevents: Pointer[size]): (errnoEnum.Value)

  def poll_oneoff(in: Int, out: Int, nsubscriptions: Int, nevents: Int) = {
    val inAdapted: Pointer[subscription] =
      new Pointer[subscription](mem.readInt(in).unsafeRunSync, (i) => subscription(mem, i), (i, r) => r.write(i, mem))
    val outAdapted: Pointer[event] =
      new Pointer[event](mem.readInt(out).unsafeRunSync, (i) => event(mem, i), (i, r) => r.write(i, mem))
    val neventsAdapted: Pointer[size] = new Pointer[size](mem.readInt(nevents).unsafeRunSync,
                                                          (i) => mem.readInt(i).unsafeRunSync,
                                                          (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      poll_oneoffImpl(inAdapted, outAdapted, nsubscriptions, neventsAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def proc_exitImpl(rval: exitcode) = {}

  def proc_exit(rval: Int) = {

    IO(proc_exitImpl(rval))
  }

  def proc_raiseImpl(sig: signalEnum.Value): (errnoEnum.Value)

  def proc_raise(sig: Int) = {
    val sigAdapted: signalEnum.Value = signalEnum(sig)

    IO(try {
      proc_raiseImpl(sigAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def sched_yieldImpl(): (errnoEnum.Value)

  def sched_yield() = {

    IO(try {
      sched_yieldImpl().id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def random_getImpl(buf: Pointer[u8], buf_len: size): (errnoEnum.Value)

  def random_get(buf: Int, buf_len: Int) = {
    val bufAdapted: Pointer[u8] = new Pointer[u8](mem.readInt(buf).unsafeRunSync,
                                                  (i) => mem.readByte(i).unsafeRunSync(),
                                                  (i, r) => mem.writeByte(i, `r`).unsafeRunSync)

    IO(try {
      random_getImpl(bufAdapted, buf_len).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def sock_recvImpl(fd: fd,
                    ri_data: iovec_array,
                    ri_dataLen: u32,
                    ri_flags: riflagsFlags.Value,
                    ro_datalen: Pointer[size],
                    ro_flags: Pointer[roflagsFlags.Value]): (errnoEnum.Value)

  def sock_recv(fd: Int, ri_data: Int, ri_dataLen: Int, ri_flags: Int, ro_datalen: Int, ro_flags: Int) = {
    val ri_dataAdapted: iovec_array = new ArrayInstance[iovec](ri_data, ri_dataLen, 8, (i) => iovec(mem, i)).values
    val ri_flagsAdapted: riflagsFlags.Value = riflagsFlags(ri_flags)
    val ro_datalenAdapted: Pointer[size] = new Pointer[size](mem.readInt(ro_datalen).unsafeRunSync,
                                                             (i) => mem.readInt(i).unsafeRunSync,
                                                             (i, r) => mem.writeInt(i, `r`).unsafeRunSync)
    val ro_flagsAdapted: Pointer[roflagsFlags.Value] = new Pointer[roflagsFlags.Value](
      mem.readInt(ro_flags).unsafeRunSync,
      (i) => roflagsFlags(mem.readShort(i).unsafeRunSync),
      (i, r) => mem.writeShort(i, `r`.id.toShort).unsafeRunSync)

    IO(try {
      sock_recvImpl(fd, ri_dataAdapted, ri_dataLen, ri_flagsAdapted, ro_datalenAdapted, ro_flagsAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def sock_sendImpl(fd: fd,
                    si_data: ciovec_array,
                    si_dataLen: u32,
                    si_flags: siflags,
                    so_datalen: Pointer[size]): (errnoEnum.Value)

  def sock_send(fd: Int, si_data: Int, si_dataLen: Int, si_flags: Int, so_datalen: Int) = {
    val si_dataAdapted: ciovec_array = new ArrayInstance[ciovec](si_data, si_dataLen, 8, (i) => ciovec(mem, i)).values
    val si_flagsAdapted: Short = si_flags.toShort
    val so_datalenAdapted: Pointer[size] = new Pointer[size](mem.readInt(so_datalen).unsafeRunSync,
                                                             (i) => mem.readInt(i).unsafeRunSync,
                                                             (i, r) => mem.writeInt(i, `r`).unsafeRunSync)

    IO(try {
      sock_sendImpl(fd, si_dataAdapted, si_dataLen, si_flagsAdapted, so_datalenAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

  def sock_shutdownImpl(fd: fd, how: sdflagsFlags.Value): (errnoEnum.Value)

  def sock_shutdown(fd: Int, how: Int) = {
    val howAdapted: sdflagsFlags.Value = sdflagsFlags(how)

    IO(try {
      sock_shutdownImpl(fd, howAdapted).id
    } catch {
      case x: WASIException => x.errno.id
    })
  }

}
