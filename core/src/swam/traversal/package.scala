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

import cats._
import cats.implicits._

import scala.collection.immutable.VectorBuilder

package object traversal {

  implicit class VectorOps[A](val v: Vector[A]) extends AnyVal {
    def mapAccumulateM[F[_], Acc, B](z: Acc)(f: (Acc, A) => F[(Acc, B)])(implicit F: Monad[F]): F[(Acc, Vector[B])] =
      v.foldM((z, new VectorBuilder[B])) {
          case ((acc, builder), a) =>
            f(acc, a).map {
              case (acc, b) => (acc, builder += b)
            }
        }
        .map {
          case (acc, vb) => (acc, vb.result)
        }
  }

}
