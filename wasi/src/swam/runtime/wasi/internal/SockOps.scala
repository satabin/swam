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

private[wasi] trait SockOps[F[_]] extends WasiBase[F] {

  def sockRecv(fd: Fd,
               riDataOffset: Pointer,
               riDataSize: Size,
               riFlags: Riflags,
               roDataSize: Pointer,
               roFlags: Pointer): F[Errno] =
    unimplemented("sock_recv")

  def sockSend(fd: Fd, siDataOffset: Pointer, siDataSize: Size, siFlags: Siflags, soDataLen: Pointer): F[Errno] =
    unimplemented("sock_send")

  def sockShutdown(fd: Fd, how: Sdflags): F[Errno] =
    unimplemented("sock_shutdown")

}
