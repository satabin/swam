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

import cats.implicits._

import java.nio.channels.FileChannel
import java.nio.file._
import java.nio.file.attribute._
import java.util.concurrent.TimeUnit

private[wasi] trait PathOps[F[_]] extends WasiBase[F] {

  def pathCreateDirectory(fd: Fd, pathOffset: Pointer, pathSize: Size): F[Errno] =
    manager.getHandle(fd, Rights.PathCreateDirectory) { handle =>
      childFile(handle.path, pathOffset, pathSize) { child =>
        for {
          res <- blocker
            .delay(Files.createDirectory(child))
            .as(success)
            .handleErrorWith(t => logger.error(s"unable to create directory $child", t).as(io))
        } yield res
      }.handleErrorWith(t => logger.error(s"unable to read directory name", t).as(io))
    }

  def pathFilestatGet(fd: Fd, flags: Lookupflags, pathOffset: Pointer, pathSize: Size, buf: Pointer): F[Errno] =
    unimplemented("path_filestat_get")

  def pathFilestatSetTimes(fd: Fd,
                           dirflags: Lookupflags,
                           pathOffset: Pointer,
                           pathSize: Size,
                           atim: Timestamp,
                           mtim: Timestamp,
                           fstFlags: Fstflags): F[Errno] =
    manager.getHandle(fd, Rights.FdFilestatSetTimes) { handle =>
      childFile(handle.path, pathOffset, pathSize) { child =>
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

          val linkOption =
            if ((dirflags & Lookupflags.SymlinkFollow) == 0)
              List(LinkOption.NOFOLLOW_LINKS)
            else
              Nil

          blocker
            .delay(
              Files
                .getFileAttributeView(child, classOf[BasicFileAttributeView], linkOption: _*)
                .setTimes(mtime, atime, null))
            .as(success)
            .handleErrorWith(t => logger.error(s"unable to set times for file $child", t).as(io))
        }
      }
    }

  def pathLink(oldFd: Fd,
               oldFlags: Lookupflags,
               oldPathOffset: Pointer,
               oldPathSize: Size,
               newFd: Fd,
               newPathOffset: Pointer,
               newPathSize: Size): F[Errno] =
    manager.getHandle(oldFd, Rights.PathLinkSource) { oldHandle =>
      manager.getHandle(newFd, Rights.PathLinkTarget) { newHandle =>
        val linkOption =
          if ((oldFlags & Lookupflags.SymlinkFollow) == 0)
            List(LinkOption.NOFOLLOW_LINKS)
          else
            Nil

        childFile(oldHandle.path, oldPathOffset, oldPathSize) { oldPath =>
          childFile(newHandle.path, newPathOffset, newPathSize) { newPath =>
            (for {
              oldPath <- blocker.delay(oldPath.toRealPath(linkOption: _*))
              _ <- blocker.delay(Files.createLink(newPath, oldPath))
            } yield success).handleErrorWith(t =>
              logger.error(s"unable to create hard link from $newPath to $oldPath", t).as(io))
          }
        }
      }
    }

  def pathOpen(fd: Fd,
               dirflags: Lookupflags,
               pathOffset: Pointer,
               pathSize: Size,
               oflags: Oflags,
               fsRightsBase: Rights,
               fsRightsInheriting: Rights,
               fdflags: FdFlags,
               opened: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.PathOpen) { parent =>
      // check that required rights are a subset of inherited ones
      if ((fsRightsBase & parent.rightsInheriting) != fsRightsBase) {
        F.pure(perm)
      } else if (Oflags.hasRights(oflags, parent.rightsBase) &&
                 FdFlags.hasRights(fdflags, parent.rightsBase)) {
        childFile(parent.path, pathOffset, pathSize) { child =>
          val handle =
            if ((oflags & Oflags.Directory) == Oflags.Directory) {
              // this is a directory, potentially to create
              val linkOption =
                if ((dirflags & Lookupflags.SymlinkFollow) == 0)
                  List(LinkOption.NOFOLLOW_LINKS)
                else
                  Nil
              val handle = Handle(Filetype.Directory, fdflags, fsRightsBase, fsRightsInheriting, child, None)
              if (Files.notExists(child, linkOption: _*) && (oflags & Oflags.Creat) == Oflags.Creat) {
                blocker
                  .delay[F, Path](Files.createDirectories(child))
                  .as(handle)
              } else {
                F.pure(handle)
              }
            } else {
              // this is a standard file, let's open the channel with the open options
              val ooptions =
                List(
                  oflags.when(Oflags.Creat, StandardOpenOption.CREATE),
                  oflags.when(Oflags.Excl, StandardOpenOption.CREATE_NEW),
                  oflags.when(Oflags.Trunc, StandardOpenOption.TRUNCATE_EXISTING),
                  fdflags.when(FdFlags.Append, StandardOpenOption.APPEND),
                  fdflags.when(FdFlags.Dsync, StandardOpenOption.DSYNC),
                  fdflags.when(FdFlags.Sync, StandardOpenOption.SYNC),
                  parent.rightsInheriting.whenOneOf(Rights.FdRead | Rights.FdReaddir, StandardOpenOption.READ),
                  parent.rightsInheriting.whenOneOf(
                    Rights.FdDatasync | Rights.FdWrite | Rights.FdAllocate | Rights.PathFilestatSetSize,
                    StandardOpenOption.WRITE)
                ).flatten

              blocker.delay[F, Handle](
                Handle(Filetype.RegularFile,
                       fdflags,
                       parent.rightsInheriting,
                       fsRightsInheriting,
                       child,
                       Some(FileChannel.open(child, ooptions: _*))))
            }

          (for {
            handle <- handle
            fd <- manager.allocateHandle(handle)
            mem <- mem.get
            res <- fd match {
              case Some(fd) => mem.writeInt(opened, fd).as(success)
              case None     => F.pure(badf)
            }
          } yield res)
            .handleErrorWith(t => logger.error(s"could not open file $child", t).as(io))
        }.handleErrorWith(t => logger.error(s"unable to read file name", t).as(io))
      } else {
        F.pure(perm)
      }
    }

  def pathReadlink(fd: Fd,
                   pathOffset: Pointer,
                   pathSize: Size,
                   buf: Pointer,
                   bufLen: Size,
                   bufused: Pointer): F[Errno] =
    manager.getHandle(fd, Rights.PathReadlink) { handle =>
      childFile(handle.path, pathOffset, pathSize) { child =>
        (for {
          target <- blocker.delay(Files.readSymbolicLink(child))
          bytes = target.toAbsolutePath().toString().getBytes("UTF-8").take(bufLen)
          mem <- mem.get
          _ <- mem.writeBytes(buf, bytes)
          _ <- mem.writeInt(bufused, bytes.length)
        } yield success).handleErrorWith(t => logger.error(s"unable to read link $child", t).as(io))
      }
    }

  def pathRemoveDirectory(fd: Fd, pathOffset: Pointer, pathSize: Size): F[Errno] =
    manager.getHandle(fd, Rights.PathRemoveDirectory) { handle =>
      childFile(handle.path, pathOffset, pathSize) { child =>
        blocker
          .delay(Files.isDirectory(child) && Files.list(child).count() == 0)
          .flatMap {
            case true =>
              blocker
                .delay(Files.delete(child))
                .as(success)
            case false =>
              F.pure(notempty)
          }
          .handleErrorWith(t => logger.error(s"unable to remove directory $child", t).as(io))
      }
    }

  def pathRename(fd: Fd,
                 oldPathOffset: Pointer,
                 oldPathSize: Size,
                 newFd: Fd,
                 newPathOffset: Pointer,
                 newPathSize: Size): F[Errno] =
    manager.getHandle(fd, Rights.PathRenameSource) { oldHandle =>
      manager.getHandle(newFd, Rights.PathRenameTarget) { newHandle =>
        childFile(oldHandle.path, oldPathOffset, oldPathSize) { oldPath =>
          childFile(newHandle.path, newPathOffset, newPathSize) { newPath =>
            blocker
              .delay(Files.move(oldPath, newPath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING))
              .as(success)
              .handleErrorWith(t => logger.error(s"unable to rename $oldPath into $newPath", t).as(io))
          }
        }
      }
    }

  def pathSymlink(oldPathOffset: Pointer,
                  oldPathSize: Size,
                  newFd: Fd,
                  newPathOffset: Pointer,
                  newPathSize: Size): F[Errno] =
    manager.getHandle(newFd, Rights.PathSymlink) { newHandle =>
      file(oldPathOffset, oldPathSize) { oldPath =>
        childFile(newHandle.path, newPathOffset, newPathSize) { newPath =>
          blocker
            .delay(Files.createSymbolicLink(newPath, oldPath))
            .as(success)
            .handleErrorWith(t => logger.error(s"unable to create hard link from $newPath to $oldPath", t).as(io))
        }
      }
    }

  def pathUnlinkFile(fd: Fd, pathOffset: Pointer, pathSize: Size): F[Errno] =
    manager.getHandle(fd, Rights.PathUnlinkFile) { handle =>
      childFile(handle.path, pathOffset, pathSize) { child =>
        blocker
          .delay(Files.isSymbolicLink(child))
          .flatMap {
            case true => blocker.delay(Files.delete(child)).as(success)
            case false =>
              blocker.delay(Files.isDirectory(child)).map {
                case true  => isdir
                case false => nolink
              }
          }
          .handleErrorWith(t => logger.error(s"unable to unlink $child", t).as(io))
      }
    }

}
