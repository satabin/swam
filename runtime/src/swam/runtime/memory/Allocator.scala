/*
 * Copyright 2019 Lucas Satabin
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
package memory

import cats._

import scala.language.higherKinds

trait Allocator[F[_]] {

  def allocate(size: Int): F[Int]

  def free(address: Int): F[Unit]

}

object Allocator {

  implicit def NoopAllocator[F[_]](implicit F: Monad[F]): Allocator[F] = new Allocator[F] {

    def allocate(size: Int): F[Int] = F.pure(-1)

    def free(address: Int): F[Unit] = F.unit

  }

}
