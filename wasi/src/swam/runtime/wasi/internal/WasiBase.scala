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

import io.odin.Logger

import java.nio.file._
import java.nio.ByteBuffer

private[wasi] abstract class WasiBase[F[_]] extends Wasi[F] {

  implicit val F: Sync[F]

  implicit val contextShift: ContextShift[F]

  implicit val clock: Clock[F]

  val args: List[String]

  val blocker: Blocker

  val logger: Logger[F]

  val manager: HandleManager[F]

  // errors
  val success: Errno = Errno.Success
  val toobig: Errno = Errno.Toobig
  val acces: Errno = Errno.Acces
  val addrinuse: Errno = Errno.Addrinuse
  val addrnotavail: Errno = Errno.Addrnotavail
  val afnosupport: Errno = Errno.Afnosupport
  val again: Errno = Errno.Again
  val already: Errno = Errno.Already
  val badf: Errno = Errno.Badf
  val badmsg: Errno = Errno.Badmsg
  val busy: Errno = Errno.Busy
  val canceled: Errno = Errno.Canceled
  val child: Errno = Errno.Child
  val connaborted: Errno = Errno.Connaborted
  val connrefused: Errno = Errno.Connrefused
  val connreset: Errno = Errno.Connreset
  val deadlk: Errno = Errno.Deadlk
  val destaddrreq: Errno = Errno.Destaddrreq
  val dom: Errno = Errno.Dom
  val dquot: Errno = Errno.Dquot
  val exist: Errno = Errno.Exist
  val fault: Errno = Errno.Fault
  val fbig: Errno = Errno.Fbig
  val hostunreach: Errno = Errno.Hostunreach
  val idrm: Errno = Errno.Idrm
  val ilseq: Errno = Errno.Ilseq
  val inprogress: Errno = Errno.Inprogress
  val intr: Errno = Errno.Intr
  val inval: Errno = Errno.Inval
  val io: Errno = Errno.Io
  val isconn: Errno = Errno.Isconn
  val isdir: Errno = Errno.Isdir
  val loop: Errno = Errno.Loop
  val mfile: Errno = Errno.Mfile
  val mlink: Errno = Errno.Mlink
  val msgsize: Errno = Errno.Msgsize
  val multihop: Errno = Errno.Multihop
  val nametoolong: Errno = Errno.Nametoolong
  val netdown: Errno = Errno.Netdown
  val netreset: Errno = Errno.Netreset
  val netunreach: Errno = Errno.Netunreach
  val nfile: Errno = Errno.Nfile
  val nobufs: Errno = Errno.Nobufs
  val nodev: Errno = Errno.Nodev
  val noent: Errno = Errno.Noent
  val noexec: Errno = Errno.Noexec
  val nolck: Errno = Errno.Nolck
  val nolink: Errno = Errno.Nolink
  val nomem: Errno = Errno.Nomem
  val nomsg: Errno = Errno.Nomsg
  val noprotoopt: Errno = Errno.Noprotoopt
  val nospc: Errno = Errno.Nospc
  val nosys: Errno = Errno.Nosys
  val notconn: Errno = Errno.Notconn
  val notdir: Errno = Errno.Notdir
  val notempty: Errno = Errno.Notempty
  val notrecoverable: Errno = Errno.Notrecoverable
  val notsock: Errno = Errno.Notsock
  val notsup: Errno = Errno.Notsup
  val notty: Errno = Errno.Notty
  val nxio: Errno = Errno.Nxio
  val overflow: Errno = Errno.Overflow
  val ownerdead: Errno = Errno.Ownerdead
  val perm: Errno = Errno.Perm
  val pipe: Errno = Errno.Pipe
  val proto: Errno = Errno.Proto
  val protonosupport: Errno = Errno.Protonosupport
  val prototype: Errno = Errno.Prototype
  val range: Errno = Errno.Range
  val rofs: Errno = Errno.Rofs
  val spipe: Errno = Errno.Spipe
  val srch: Errno = Errno.Srch
  val stale: Errno = Errno.Stale
  val timedout: Errno = Errno.Timedout
  val txtbsy: Errno = Errno.Txtbsy
  val xdev: Errno = Errno.Xdev
  val notcapable: Errno = Errno.Notcapable

  def unimplemented(name: String) =
    logger.error(s"$name not implemented").as(nosys)

  def file(pathOffset: Pointer, pathSize: Size)(f: Path => F[Errno]): F[Errno] =
    // read the path from memory
    mem.get.flatMap(
      _.readBytes(pathOffset, pathSize)
        .flatMap { bytes =>
          val pathStr = new String(bytes, "UTF-8")
          f(Paths.get(pathStr))
        }
        .handleErrorWith(t => logger.error("unable to read file name", t).as(io)))

  def childFile(parent: Path, pathOffset: Pointer, pathSize: Size)(f: Path => F[Errno]): F[Errno] =
    // read the path from memory
    mem.get.flatMap(
      _.readBytes(pathOffset, pathSize)
        .flatMap { bytes =>
          val pathStr = new String(bytes, "UTF-8")
          val child = parent.resolve(pathStr).normalize()
          if (!child.startsWith(parent)) {
            // the file is not a child of the file descriptor, no capability
            F.pure(notcapable)
          } else {
            f(child)
          }
        }
        .handleErrorWith(t => logger.error(s"unable to read child of $parent file name", t).as(io)))

  def readBuffersAndOffsets(iovs: Pointer, iovsSize: Size): F[(Array[ByteBuffer], Array[Int])] =
    F.tailRecM((0, Array.ofDim[ByteBuffer](iovsSize), Array.ofDim[Int](iovsSize))) {
      case (idx, buffers, offsets) =>
        if (idx >= iovsSize) {
          F.pure((buffers, offsets).asRight[(Int, Array[ByteBuffer], Array[Int])])
        } else {
          for {
            mem <- mem.get
            iovOffset <- mem.readInt(iovs + idx * 8)
            iovSize <- mem.readInt(iovs + idx * 8 + 4)
          } yield {
            buffers(idx) = ByteBuffer.wrap(Array.ofDim[Byte](iovSize))
            offsets(idx) = iovOffset
            (idx + 1, buffers, offsets).asLeft[(Array[ByteBuffer], Array[Int])]
          }
        }
    }

  def writeBuffers(iovs: Pointer, iovsSize: Size): F[Array[ByteBuffer]] =
    mem.get.flatMap { mem =>
      F.tailRecM((0, Array.ofDim[ByteBuffer](iovsSize))) {
        case (idx, buffers) =>
          if (idx >= iovsSize) {
            F.pure(buffers.asRight[(Int, Array[ByteBuffer])])
          } else {
            for {
              iovOffset <- mem.readInt(iovs + idx * 8)
              iovSize <- mem.readInt(iovs + idx * 8 + 4)
              buffer <- mem.readBytes(iovOffset, iovSize)
            } yield {
              buffers(idx) = ByteBuffer.wrap(buffer)
              (idx + 1, buffers).asLeft[Array[ByteBuffer]]
            }
          }
      }
    }

}
