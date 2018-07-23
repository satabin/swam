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

import formats._

import cats._

import scala.language.higherKinds

package object functions {

  def importable[F[_]](f: () => F[Unit])(implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function0UnitImportable[F](f)

  def importable[F[_], Ret: ValueWriter](f: () => F[Ret])(implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function0Importable[F, Ret](f)

  def importable[F[_], P1: ValueReader](f: (P1) => F[Unit])(implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function1UnitImportable[F, P1](f)

  def importable[F[_], P1: ValueReader, Ret: ValueWriter](f: (P1) => F[Ret])(
      implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function1Importable[F, P1, Ret](f)

  def importable[F[_], P1: ValueReader, P2: ValueReader](f: (P1, P2) => F[Unit])(
      implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function2UnitImportable[F, P1, P2](f)

  def importable[F[_], P1: ValueReader, P2: ValueReader, Ret: ValueWriter](f: (P1, P2) => F[Ret])(
      implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function2Importable[F, P1, P2, Ret](f)

  def importable[F[_], P1: ValueReader, P2: ValueReader, P3: ValueReader](f: (P1, P2, P3) => F[Unit])(
      implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function3UnitImportable[F, P1, P2, P3](f)

  def importable[F[_], P1: ValueReader, P2: ValueReader, P3: ValueReader, Ret: ValueWriter](f: (P1, P2, P3) => F[Ret])(
      implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function3Importable[F, P1, P2, P3, Ret](f)

  def importable[F[_], P1: ValueReader, P2: ValueReader, P3: ValueReader, P4: ValueReader](
      f: (P1, P2, P3, P4) => F[Unit])(implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function4UnitImportable[F, P1, P2, P3, P4](f)

  def importable[F[_], P1: ValueReader, P2: ValueReader, P3: ValueReader, P4: ValueReader, Ret: ValueWriter](
      f: (P1, P2, P3, P4) => F[Ret])(implicit F: MonadError[F, Throwable]): Importable[F] =
    new Function4Importable[F, P1, P2, P3, P4, Ret](f)

}
