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

import cats.effect._
import cats.implicits._

import fs2._

import java.nio.file.Path

package object text {

  def readFile[F[_]](path: Path, blocker: Blocker, chunkSize: Int)(implicit F: Sync[F],
                                                                   cs: ContextShift[F]): F[String] =
    io.file.readAll[F](path, blocker, chunkSize).through(text.utf8Decode).compile.foldMonoid

}
