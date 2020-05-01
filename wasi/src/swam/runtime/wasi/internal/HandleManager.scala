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

import cats.effect._
import cats.implicits._
import cats.effect.concurrent.Ref

import io.odin.Logger

import java.nio.file._
import java.nio.channels._

private case class Handle(filetype: Filetype,
                          flags: FdFlags,
                          rightsBase: Rights,
                          rightsInheriting: Rights,
                          path: Path,
                          channel: Option[Channel])

private case class Handles(descriptors: Vector[Option[Handle]], nextOpen: Int, freed: List[Int])

private[wasi] class HandleManager[F[_]](private[internal] val handles: Ref[F, Handles],
                                        blocker: Blocker,
                                        logger: Logger[F])(implicit F: Sync[F], cs: ContextShift[F]) {

  def allocateHandle(handle: Handle): F[Option[Fd]] =
    handles.modify {
      case Handles(descriptors, nextOpen, free :: freed) =>
        // reuse a previously freed descriptors
        (Handles(descriptors.updated(free, Some(handle)), nextOpen, freed), Some(free))
      case files @ Handles(descriptors, nextOpen, Nil) =>
        if (nextOpen >= descriptors.length)
          // no more available files
          (files, None)
        else
          // allocate at the endsWith
          (Handles(descriptors.updated(nextOpen, Some(handle)), nextOpen + 1, Nil), Some(nextOpen))
    }

  def deallocateHandle(fd: Fd): F[Errno] =
    handles
      .modify {
        case files @ Handles(descriptors, nextOpen, freed) =>
          if (fd < 0 || fd >= nextOpen) {
            (files, None)
          } else {
            val h = descriptors(fd)
            (Handles(descriptors.updated(fd, None), nextOpen, fd :: freed), h)
          }
      }
      .flatMap {
        case Some(Handle(_, _, _, _, path, Some(channel))) =>
          blocker
            .delay[F, Unit](channel.close())
            .as(Errno.Success: Errno)
            .handleErrorWith(t => logger.error(s"unable to close channel for file $path", t).as(Errno.Io))
        case Some(_) => F.pure(Errno.Success)
        case None    => F.pure(Errno.Badf)
      }

  def getHandle(fd: Fd, rights: Rights)(f: Handle => F[Errno]): F[Errno] =
    handles.get.map(_.descriptors.lift(fd).flatten).flatMap {
      case Some(handle) =>
        if ((handle.rightsBase & rights) != rights) {
          F.pure(Errno.Perm)
        } else {
          f(handle)
        }
      case None =>
        F.pure(Errno.Badf)
    }

  def modifyHandle(fd: Fd)(f: Handle => (Handle, Errno)): F[Errno] =
    handles.modify { handles =>
      handles.descriptors.lift(fd).flatten match {
        case Some(handle) =>
          val (newHandle, errno) = f(handle)
          (handles.copy(descriptors = handles.descriptors.updated(fd, Some(newHandle))), errno)
        case None =>
          (handles, Errno.Badf)
      }
    }

  def close(): F[Unit] =
    handles.get.flatMap {
      case Handles(descriptors, nextOpen, _) =>
        // do not close the three first channels,
        // they are special cases
        F.tailRecM(3) { idx =>
          if (idx >= nextOpen) {
            F.pure(Right(()))
          } else {
            descriptors(idx) match {
              case Some(Handle(_, _, _, _, path, Some(channel))) =>
                blocker
                  .delay[F, Unit](channel.close())
                  .handleErrorWith(t => logger.error(s"error while closing channel for $path", t))
                  .as(Left(idx + 1))
              case _ =>
                F.pure(Left(idx + 1))
            }
          }
        }
    }

}

private[wasi] object HandleManager {

  val RegularFileBaseRights: Rights = Rights.FdDatasync |
    Rights.FdRead |
    Rights.FdSeek |
    Rights.FdFdstatSetFlags |
    Rights.FdSync |
    Rights.FdTell |
    Rights.FdWrite |
    Rights.FdAdvise |
    Rights.FdAllocate |
    Rights.FdFilestatGet |
    Rights.FdFilestatSetSize |
    Rights.FdFilestatSetTimes |
    Rights.PollFdReadwrite

  val RegularFilesInheritingRights: Rights = 0

  val DirectoryBaseRights: Rights = Rights.FdFdstatSetFlags |
    Rights.FdSync |
    Rights.FdAdvise |
    Rights.PathCreateDirectory |
    Rights.PathCreateFile |
    Rights.PathLinkSource |
    Rights.PathLinkTarget |
    Rights.PathOpen |
    Rights.FdReaddir |
    Rights.PathReadlink |
    Rights.PathRenameSource |
    Rights.PathRenameTarget |
    Rights.PathFilestatGet |
    Rights.PathFilestatSetSize |
    Rights.PathFilestatSetTimes |
    Rights.FdFilestatGet |
    Rights.FdFilestatSetTimes |
    Rights.PathSymlink |
    Rights.PathUnlinkFile |
    Rights.PathRemoveDirectory |
    Rights.PollFdReadwrite

  val DirectoryInheritingRights: Rights = DirectoryBaseRights | RegularFileBaseRights

  def apply[F[_]: ContextShift](blocker: Blocker, preopenedDirs: List[Path], logger: Logger[F])(
      implicit F: Sync[F]): Resource[F, HandleManager[F]] =
    Resource.make(
      Ref
        .of(Handles(Vector.fill(256)(None), 0, Nil))
        .flatTap(addPreopened(blocker, _, preopenedDirs))
        .map(new HandleManager[F](_, blocker, logger)))(_.close())

  // some file descriptors are expected to be preopened:
  //  - 0 is stdin
  //  - 1 is stdout
  //  - 2 is stderr
  private def addPreopened[F[_]: ContextShift](blocker: Blocker, handles: Ref[F, Handles], preopenedDirs: List[Path])(
      implicit F: Sync[F]): F[Unit] =
    handles.get.flatMap[Unit](handles =>
      if (preopenedDirs.size >= handles.descriptors.size - 3)
        F.raiseError(new WasiException(s"too many preopened directories"))
      else
        F.unit) >>
      preopenedDirs
        .traverse { dir =>
          blocker.delay[F, Boolean](Files.isDirectory(dir)).flatMap[Path] {
            case true  => blocker.delay(dir.toRealPath().toAbsolutePath())
            case false => F.raiseError(new WasiException(s"$dir is not a directory"))
          }
        }
        .flatMap { preopenedDirs =>
          handles.update { handles =>
            val stdin =
              Handle(Filetype.Unknown, 0, Rights.FdRead, 0, Paths.get("<stdin>"), Some(Channels.newChannel(System.in)))
            val stdout =
              Handle(Filetype.Unknown,
                     0,
                     Rights.FdWrite,
                     0,
                     Paths.get("<stdout>"),
                     Some(Channels.newChannel(System.out)))
            val stderr =
              Handle(Filetype.Unknown,
                     0,
                     Rights.FdWrite,
                     0,
                     Paths.get("<stderr>"),
                     Some(Channels.newChannel(System.err)))
            val withstd = handles.descriptors
              .updated(0, Some(stdin))
              .updated(1, Some(stdout))
              .updated(2, Some(stderr))
            val descriptors = preopenedDirs.zipWithIndex.foldLeft(withstd) {
              case (descriptors, (dir, idx)) =>
                descriptors
                  .updated(
                    idx + 3,
                    Some(Handle(Filetype.Directory, 0, DirectoryBaseRights, DirectoryInheritingRights, dir, None)))
            }
            handles.copy(descriptors = descriptors, nextOpen = 3 + preopenedDirs.size)
          }
        }

}
