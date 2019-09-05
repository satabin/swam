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
package formats

import cats._

/** A formatter is both a [[ValueReader]] and a [[ValueWriter]].
  */
trait ValueFormatter[F[_], T] extends ValueReader[F, T] with ValueWriter[F, T]

abstract class SimpleValueFormatter[F[_], T](implicit F: MonadError[F, Throwable])
    extends SimpleValueWriter[F, T]
    with SimpleValueReader[F, T]
    with ValueFormatter[F, T]
