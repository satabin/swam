/*
 * Copyright 2018 Lucas Satabin
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
package exports

import formats._

import cats._

trait EFunction[Ret, F[_]] {

  protected def wrap(res: Option[Value]): F[Ret]

}

private[exports] object EFunction {

  def wrapUnit[F[_]](res: Option[Value])(implicit F: MonadError[F, Throwable]): F[Unit] =
    res match {
      case None =>
        F.pure(())
      case Some(_) =>
        throw new Exception("This is a bug")
    }

  def wrap[F[_], Ret](res: Option[Value], m: Option[Memory[F]])(implicit F: MonadError[F, Throwable],
                                                                reader: ValueReader[F, Ret]): F[Ret] =
    res match {
      case Some(ret) =>
        reader.read(ret, m)
      case None =>
        throw new Exception("This is a bug")
    }
}
