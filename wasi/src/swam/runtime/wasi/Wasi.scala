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

import internal._
import imports.annotations._

import cats.effect._
import cats.effect.concurrent.Deferred

import io.odin.Logger

import java.nio.file.Path

/** Interface of the WASI API, as of snapshot1
  */
@module
abstract class Wasi[@effect F[_]] {

  val mem: Deferred[F, Memory[F]]

  @effectful(name = "args_get")
  def argsGet(argv: Pointer, argvBuf: Pointer): F[Errno]

  @effectful(name = "args_sizes_get")
  def argsSizesGet(argc: Pointer, argvBufSize: Pointer): F[Errno]

  @effectful(name = "environ_get")
  def environGet(environ: Pointer, buf: Pointer): F[Errno]

  @effectful(name = "environ_sizes_get")
  def environSizesGet(environc: Pointer, environBufSize: Pointer): F[Errno]

  @effectful(name = "fd_advise")
  def fdAdvise(fd: Fd, offset: Filesize, len: Filesize, advice: Advice): F[Errno]

  @effectful(name = "fd_allocate")
  def fdAllocate(fd: Fd, offset: Filesize, len: Filesize): F[Errno]

  @effectful(name = "fd_close")
  def fdClose(fd: Fd): F[Errno]

  @effectful(name = "fd_datasync")
  def fdDatasync(fd: Fd): F[Errno]

  @effectful(name = "fd_fdstat_get")
  def fdFdstatGet(fd: Fd, ptr: Pointer): F[Errno]

  @effectful(name = "fd_fdstat_set_flags")
  def fdFdstatSetFlags(fd: Fd, flags: FdFlags): F[Errno]

  @effectful(name = "fd_fdstat_set_rights")
  def fdFdstatSetRights(fd: Fd, fsRightsBase: Rights, fsRightsInheriting: Rights): F[Errno]

  @effectful(name = "fd_filestat_get")
  def fdFilestatGet(fd: Fd, ptr: Pointer): F[Errno]

  @effectful(name = "fd_filestat_set_size")
  def fdFilestatSetSize(fd: Fd, size: Filesize): F[Errno]

  @effectful(name = "fd_filestat_set_times")
  def fdFilestatSetTimes(fd: Fd, atim: Timestamp, mtim: Timestamp, fstFlags: Fstflags): F[Errno]

  @effectful(name = "fd_pread")
  def fdPread(fd: Fd, iovs: Pointer, iovsSize: Size, offset: Filesize, nread: Pointer): F[Errno]

  @effectful(name = "fd_prestat_get")
  def fdPrestatGet(fd: Fd, prestat: Pointer): F[Errno]

  @effectful(name = "fd_prestat_dir_name")
  def fdPrestatDirName(fd: Fd, path: Pointer, pathLen: Size): F[Errno]

  @effectful(name = "fd_pwrite")
  def fdPwrite(fd: Fd, iovs: Pointer, iovsSize: Size, offset: Filesize, nwritten: Pointer): F[Errno]

  @effectful(name = "fd_read")
  def fdRead(fd: Fd, iovs: Pointer, iovsSize: Size, nread: Pointer): F[Errno]

  @effectful(name = "fd_readdir")
  def fdReaddir(fd: Fd, buf: Pointer, bufLen: Size, dircookie: Dircookie, bufused: Size): F[Errno]

  @effectful(name = "fd_renumber")
  def fdRenumber(fd: Fd, to: Fd): F[Errno]

  @effectful(name = "fd_seek")
  def fdSeek(fd: Fd, offset: Filedelta, whence: Whence, newOffset: Pointer): F[Errno]

  @effectful(name = "fd_sync")
  def fdSync(fd: Fd): F[Errno]

  @effectful(name = "fd_tell")
  def fdTell(fd: Fd, offset: Pointer): F[Errno]

  @effectful(name = "fd_write")
  def fdWrite(fd: Fd, iovs: Pointer, iovsSize: Size, nwritten: Pointer): F[Errno]

  @effectful(name = "path_create_directory")
  def pathCreateDirectory(fd: Fd, pathOffset: Pointer, pathSize: Size): F[Errno]

  @effectful(name = "path_filestat_get")
  def pathFilestatGet(fd: Fd, flags: Lookupflags, pathOffset: Pointer, pathSize: Size, buf: Pointer): F[Errno]

  @effectful(name = "path_filestat_set_times")
  def pathFilestatSetTimes(fd: Fd,
                           dirflags: Lookupflags,
                           pathOffset: Pointer,
                           pathSize: Size,
                           atim: Timestamp,
                           mtim: Timestamp,
                           fstFlags: Fstflags): F[Errno]

  @effectful(name = "path_link")
  def pathLink(oldFd: Fd,
               oldFlags: Lookupflags,
               oldPathOffset: Pointer,
               oldPathSize: Size,
               newFd: Fd,
               newPathOffset: Pointer,
               newPathSize: Size): F[Errno]

  @effectful(name = "path_open")
  def pathOpen(fd: Fd,
               dirflags: Lookupflags,
               pathOffset: Pointer,
               pathSize: Size,
               oflags: Oflags,
               fsRightsBase: Rights,
               fsRightsInheriting: Rights,
               fdflags: FdFlags,
               opened: Pointer): F[Errno]

  @effectful(name = "path_readlink")
  def pathReadlink(fd: Fd, pathOffset: Pointer, pathSize: Size, buf: Pointer, bufLen: Size, bufused: Pointer): F[Errno]

  @effectful(name = "path_remove_directory")
  def pathRemoveDirectory(fd: Fd, pathOffset: Pointer, pathSize: Size): F[Errno]

  @effectful(name = "path_rename")
  def pathRename(fd: Fd,
                 oldPathOffset: Pointer,
                 oldPathSize: Size,
                 newFd: Fd,
                 newPathOffset: Pointer,
                 newPathSize: Size): F[Errno]

  @effectful(name = "path_symlink")
  def pathSymlink(oldPathOffset: Pointer,
                  oldPathSize: Size,
                  newFd: Fd,
                  newPathOffset: Pointer,
                  newPathSize: Size): F[Errno]

  @effectful(name = "path_unlink_file")
  def pathUnlinkFile(fd: Fd, pathOffset: Pointer, pathSize: Size): F[Errno]

  @effectful(name = "sock_recv")
  def sockRecv(fd: Fd,
               riDataOffset: Pointer,
               riDataSize: Size,
               riFlags: Riflags,
               roDataSize: Pointer,
               roFlags: Pointer): F[Errno]

  @effectful(name = "sock_send")
  def sockSend(fd: Fd, siDataOffset: Pointer, siDataSize: Size, siFlags: Siflags, soDataLen: Pointer): F[Errno]

  @effectful(name = "sock_shutdown")
  def sockShutdown(fd: Fd, how: Sdflags): F[Errno]

  @effectful(name = "poll_oneoff")
  def pollOneoff(in: Pointer, out: Pointer, nsubscriptions: Size, nevents: Pointer): F[Errno]

  @effectful(name = "proc_exit")
  def procExit(rval: Exitcode): F[Unit]

  @effectful(name = "proc_raise")
  def procRaise(sig: Signal): F[Errno]

  @effectful(name = "sched_yield")
  def schedYield(): F[Errno]

  @effectful(name = "random_get")
  def randomGet(buf: Pointer, bufLen: Size): F[Errno]

}

object Wasi {

  def apply[F[_]](preopenedDirs: List[Path], args: List[String], logger: Logger[F], blocker: Blocker)(
      implicit F: Concurrent[F],
      clock: Clock[F],
      cs: ContextShift[F]): Resource[F, Wasi[F]] =
    Resource.liftF(Deferred[F, Memory[F]]).flatMap { mem =>
      HandleManager[F](blocker, preopenedDirs, logger).map(new WasiImpl[F](args, _, mem, blocker, logger))
    }

}
