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
package internal

import cats.effect.Resource
import cats.implicits._

import java.nio.channels._
import java.nio.file._
import java.nio.file.attribute._
import java.util.concurrent.TimeUnit

private[wasi] trait FileOps[F[_]] extends WasiBase[F] {

  def fdAdvise(fd: Fd, offset: Filesize, len: Filesize, advice: Advice): F[Errno] =
    unimplemented("fd_advise")

  def fdAllocate(fd: Fd, offset: Filesize, len: Filesize): F[Errno] =
    unimplemented("fd_allocate")

  def fdClose(fd: Fd): F[Errno] =
    manager.getHandle(fd, Rights.FdDatasync) { handle => manager.deallocateHandle(fd) }

  def fdDatasync(fd: Fd): F[Errno] =
    manager.getHandle(fd, Rights.FdDatasync) { handle =>
      handle.channel match {
        case Some(channel: FileChannel) =>
          blocker
            .delay(channel.force(false))
            .as(success)
            .handleErrorWith(t => logger.error(s"unable to datasync file ${handle.path}", t).as(io))
        case Some(_) =>
          F.raiseError(new WasiException(s"fd ${handle.path} should be a file. This is a bug"))
        case None =>
          F.pure(isdir)
      }
    }

  def fdFdstatGet(fd: Fd, ptr: Pointer): F[Errno] =
    manager.getHandle(fd, 0) { handle =>
      (for {
        mem <- mem.get
        _ <- mem.writeByte(ptr, handle.filetype.value)
        _ <- mem.writeShort(ptr + 1, handle.flags)
        _ <- mem.writeLong(ptr + 1 + 2, handle.rightsBase)
        _ <- mem.writeLong(ptr + 1 + 2 + 8, handle.rightsInheriting)
      } yield success).handleErrorWith(t => logger.error("unable to write fd stat", t).as(io))
    }

  def fdFdstatSetFlags(fd: Fd, flags: FdFlags): F[Errno] =
    unimplemented("fd_fdstat_set_flags")

  def fdFdstatSetRights(fd: Fd, fsRightsBase: Rights, fsRightsInheriting: Rights): F[Errno] =
    manager.modifyHandle(fd) { handle =>
      if ((fsRightsBase & handle.rightsBase) != fsRightsBase || (fsRightsInheriting & handle.rightsInheriting) != fsRightsInheriting) {
        // can only remove rights, not add any
        (handle, notcapable)
      } else {
        (handle.copy(rightsBase = fsRightsBase, rightsInheriting = fsRightsInheriting), success)
      }
    }

  def fdFilestatGet(fd: Fd, ptr: Pointer): F[Errno] =
    unimplemented("fd_filestat_get")

  def fdFilestatSetSize(fd: Fd, size: Filesize): F[Errno] =
    manager.getHandle(fd, Rights.FdFilestatSetSize) { handle =>
      handle match {
        case Handle(_, _, _, _, path, Some(channel: SeekableByteChannel)) =>
          blocker.delay(channel.truncate(size)).as(success).handleErrorWith { t =>
            logger
              .error(s"unable to truncate file $path", t)
              .as(t match {
                case _: NonWritableChannelException => perm
                case _                              => io
              })
          }
        case Handle(_, _, _, _, _, Some(_)) =>
          F.raiseError(new WasiException(s"fd ${handle.path} should be a file. This is a bug"))
        case Handle(_, _, _, _, path, _) =>
          logger.error(s"unable to truncate directory $path").as(io)
      }
    }

  def fdFilestatSetTimes(fd: Fd, atim: Timestamp, mtim: Timestamp, fstFlags: Fstflags): F[Errno] =
    manager.getHandle(fd, Rights.FdFilestatSetTimes) { handle =>
      clock.realTime(TimeUnit.MILLISECONDS).flatMap { now =>
        val atime =
          if ((fstFlags & Fstflags.AtimNow) == Fstflags.AtimNow)
            FileTime.fromMillis(now)
          else if ((fstFlags & Fstflags.Atim) == Fstflags.Atim)
            FileTime.fromMillis(atim)
          else
            null

        val mtime =
          if ((fstFlags & Fstflags.MtimNow) == Fstflags.MtimNow)
            FileTime.fromMillis(now)
          else if ((fstFlags & Fstflags.Mtim) == Fstflags.Mtim)
            FileTime.fromMillis(mtim)
          else
            null
        blocker
          .delay(
            Files
              .getFileAttributeView(handle.path, classOf[BasicFileAttributeView])
              .setTimes(mtime, atime, null))
          .as(success)
          .handleErrorWith(t => logger.error(s"unable to set times for file ${handle.path}", t).as(io))
      }
    }

  def fdPread(fd: Fd, iovs: Pointer, iovsSize: Size, offset: Filesize, nread: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.FdRead & Rights.FdSeek) {
      case Handle(_, _, rights, _, path, channel) =>
        channel match {
          case Some(channel: FileChannel) =>
            // read the buffer sizes and offset
            val buffersAndOffsets = readBuffersAndOffsets(iovs, iovsSize)
            (for {
              mem <- mem.get
              (buffers, offsets) <- buffersAndOffsets
              saved <- blocker.delay(channel.position())
              _ <- blocker.delay(channel.position(offset))
              read <- blocker.delay(channel.read(buffers))
              _ <- blocker.delay(channel.position(saved))
              _ <- (buffers.zip(offsets)).toVector.foldLeftM(()) {
                case ((), (buffer, offset)) => mem.writeBytes(offset, buffer.position(0))
              }
              _ <- mem.writeInt(nread, read.toInt)
            } yield success).handleErrorWith(t => logger.error(s"could not read from file $path", t).as(io))
          case Some(_) =>
            F.raiseError(new WasiException(s"fd $path should be a file. This is a bug"))
          case None =>
            F.pure(isdir)
        }
    }

  def fdPrestatGet(fd: Fd, prestat: Pointer): F[Errno] =
    manager.getHandle(fd, 0) {
      case Handle(Filetype.Directory, _, _, _, path, _) =>
        val bytes = path.toString.getBytes("UTF-8").size
        for {
          mem <- mem.get
          _ <- mem.writeByte(prestat, 0) // union tag for preopened dir
          _ <- mem.writeInt(prestat + 1, bytes)
        } yield success
      case _ =>
        F.pure(notdir)
    }

  def fdPrestatDirName(fd: Fd, pathOffset: Pointer, pathLen: Size): F[Errno] =
    manager.getHandle(fd, 0) {
      case Handle(Filetype.Directory, _, _, _, path, _) =>
        val bytes = path.toString.getBytes("UTF-8").take(pathLen)
        for {
          mem <- mem.get
          _ <- mem.writeBytes(pathOffset, bytes)
        } yield success
      case _ =>
        F.pure(notdir)
    }

  def fdPwrite(fd: Fd, iovs: Pointer, iovsSize: Size, offset: Filesize, nwritten: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.FdWrite & Rights.FdSeek) {
      case Handle(_, _, rights, _, path, channel) =>
        channel match {
          case Some(channel: FileChannel) =>
            (for {
              mem <- mem.get
              buffers <- writeBuffers(iovs, iovsSize)
              written <- blocker.delay(channel.write(buffers))
              _ <- mem.writeInt(nwritten, written.toInt)
            } yield success).handleErrorWith(t => logger.error(s"could not write to file $path", t).as(io))
          case Some(_) =>
            F.raiseError(new WasiException(s"fd $path should be a file. This is a bug"))
          case None =>
            F.pure(isdir)
        }
    }

  def fdRead(fd: Fd, iovs: Pointer, iovsSize: Size, nread: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.FdRead) {
      case Handle(_, _, rights, _, path, channel) =>
        channel match {
          case Some(channel: ScatteringByteChannel) =>
            // read the buffer sizes and offset
            val buffersAndOffsets = readBuffersAndOffsets(iovs, iovsSize)
            (for {
              mem <- mem.get
              (buffers, offsets) <- buffersAndOffsets
              read <- blocker.delay(channel.read(buffers)).map(math.max(_, 0))
              _ <- (buffers.zip(offsets)).toVector.foldLeftM(()) {
                case ((), (buffer, offset)) => mem.writeBytes(offset, buffer.position(0))
              }
              _ <- mem.writeInt(nread, read.toInt)
            } yield success).handleErrorWith(t => logger.error(s"could not read from file $path", t).as(io))
          case Some(channel: ReadableByteChannel) =>
            // let's emulate the behavior of a scattering channel
            val buffersAndOffsets = readBuffersAndOffsets(iovs, iovsSize)
            (for {
              (buffers, offsets) <- buffersAndOffsets
              read <- buffers.toVector.foldLeftM(0L) { (read, buffer) =>
                blocker.delay(channel.read(buffer)).map(math.max(_, 0)).map(_ + read)
              }
              mem <- mem.get
              _ <- (buffers.zip(offsets)).toVector.foldLeftM(()) {
                case ((), (buffer, offset)) => mem.writeBytes(offset, buffer.position(0))
              }
              _ <- mem.writeInt(nread, read.toInt)
            } yield success).handleErrorWith(t => logger.error(s"could not read from file $path", t).as(io))
          case Some(_) =>
            F.raiseError(new WasiException(s"fd $path should be readable. This is a bug"))
          case None =>
            F.pure(isdir)
        }
    }

  def fdReaddir(fd: Fd, buf: Pointer, bufLen: Size, dircookie: Dircookie, bufused: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.FdReaddir) { handle =>
      handle.filetype match {
        case Filetype.Directory =>
          blocker
            .blockOn(Resource.make(F.delay(Files.list(handle.path)))(stream => F.delay(stream.close())).use { stream =>
              F.delay(stream.skip(dircookie).toArray(Array.ofDim[Path](_)))
            })
            .flatMap { entries =>
              mem.get.flatMap { mem =>
                F.tailRecM((0, 0, buf, bufLen)) {
                  case (idx, buf, bufused, bufLen) =>
                    if (idx >= entries.length || bufLen == 0) {
                      mem.writeInt(bufused, bufused).as(success.asRight[(Int, Int, Int, Int)])
                    } else {
                      val entry = entries(idx)
                      val nameBytes = entry.getFileName().toString.getBytes("UTF-8").take(bufLen)
                      val inode = blocker.delay(
                        Option(Files.getAttribute(entry, "unix:ino").asInstanceOf[java.lang.Long]).map(_.longValue()))
                      for {
                        _ <- mem.writeLong(buf, dircookie + idx + 1)
                        _ <- inode.flatMap {
                          case Some(inode) => mem.writeLong(buf + 8, inode)
                          case None        => F.unit
                        }
                        _ <- mem.writeInt(buf + 16, nameBytes.length)
                        _ <- mem.writeByte(buf + 20, handle.filetype.value)
                        _ <- mem.writeBytes(buf + 21, nameBytes)
                      } yield (idx + 1,
                               buf + 21 + nameBytes.length,
                               bufused + 21 + nameBytes.length,
                               bufLen - nameBytes.length).asLeft[Errno]
                    }
                }
              }
            }
            .handleErrorWith(t => logger.error(s"unable to read directory ${handle.path}", t).as(io))
        case _ =>
          F.pure(notdir)
      }
    }

  def fdRenumber(fd: Fd, to: Fd): F[Errno] =
    manager.handles
      .modify {
        case files @ Handles(descriptors, lastOpen, freed) =>
          if (fd < 0 || fd >= lastOpen || to < 0 || to >= lastOpen) {
            (files, None)
          } else {
            descriptors(to) match {
              case oldTo @ Some(_) =>
                val h = descriptors(fd)
                (Handles(descriptors.updated(fd, None).updated(to, h), lastOpen, fd :: freed), oldTo)
              case None =>
                (files, None)
            }
          }
      }
      .flatMap {
        case None => F.pure(badf)
        case Some(Handle(_, _, _, _, path, Some(channel))) =>
          blocker
            .delay[F, Unit](channel.close())
            .as(success)
            .handleErrorWith(t => logger.error(s"unable to close channel for file $path", t).as(io))
        case Some(_) => F.pure(success)
      }

  def fdSeek(fd: Fd, offset: Filedelta, whence: Whence, newOffset: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.FdSeek) { handle =>
      handle match {
        case Handle(_, _, _, _, path, Some(channel: SeekableByteChannel)) =>
          val newo = whence match {
            case Whence.Set => F.pure(offset)
            case Whence.Cur => blocker.delay(channel.position()).map(_ + offset)
            case Whence.End => blocker.delay(channel.size()).map(_ + offset)
          }
          (for {
            newo <- newo
            mem <- mem.get
            _ <- mem.writeLong(newOffset, newo)
          } yield success).handleErrorWith(t => logger.error(s"unable to seek file $path", t).as(io))
        case Handle(_, _, _, _, path, Some(_)) =>
          F.raiseError(new WasiException(s"fd $path should be seekable. This is a bug"))
        case _ =>
          F.pure(isdir)
      }
    }

  def fdSync(fd: Fd): F[Errno] =
    manager.getHandle(fd, Rights.FdSync) { handle =>
      handle.channel match {
        case Some(channel: FileChannel) =>
          blocker
            .delay(channel.force(true))
            .as(success)
            .handleErrorWith(t => logger.error(s"unable to sync file ${handle.path}", t).as(io))
        case Some(_) =>
          F.raiseError(new WasiException(s"fd ${handle.path} should be a file. This is a bug"))
        case None =>
          F.pure(isdir)
      }
    }

  def fdTell(fd: Fd, offset: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.FdTell) { handle =>
      handle.channel match {
        case Some(channel: SeekableByteChannel) =>
          (for {
            position <- blocker.delay(channel.position())
            mem <- mem.get
            _ <- mem.writeLong(offset, position)
          } yield success)
            .handleErrorWith(t => logger.error(s"unable to get offset for file ${handle.path}", t).as(io))
        case Some(_) =>
          F.raiseError(new WasiException(s"fd ${handle.path} should be seekable. This is a bug"))
        case None =>
          F.pure(isdir)
      }
    }

  def fdWrite(fd: Fd, iovs: Pointer, iovsSize: Size, nwritten: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.FdWrite) {
      case Handle(_, _, rights, _, path, channel) =>
        channel match {
          case Some(channel: GatheringByteChannel) =>
            (for {
              mem <- mem.get
              buffers <- writeBuffers(iovs, iovsSize)
              written <- blocker.delay(channel.write(buffers))
              _ <- mem.writeInt(nwritten, written.toInt)
            } yield success).handleErrorWith(t => logger.error(s"could not write to file $path", t).as(io))
          case Some(channel: WritableByteChannel) =>
            // lets emulate the behavior of a gathering channel
            (for {
              mem <- mem.get
              buffers <- writeBuffers(iovs, iovsSize)
              written <- buffers.toVector.foldLeftM(0L) { (nwritten, buffer) =>
                blocker.delay(channel.write(buffer)).map(_ + nwritten)
              }
              _ <- mem.writeInt(nwritten, written.toInt)
            } yield success).handleErrorWith(t => logger.error(s"could not write to file $path", t).as(io))
          case Some(_) =>
            F.raiseError(new WasiException(s"fd $path should be writable. This is a bug"))
          case None =>
            F.pure(isdir)
        }
    }

}
