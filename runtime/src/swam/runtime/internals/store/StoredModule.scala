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
package internals
package store

import scodec._
import scodec.codecs._

import shapeless._

/**
  *  Memory representation of a module is as follows:
  *   - (int32 int32): min (-1 if no table)  and max (-1 if no max) for table
  *   - (int32 int32): min (-1 if no memory) and max (-1 if no max) for memory
  *   - int32: start function address (-1 if no start section)
  *   - int32: number n of global variables in this module
  *   - n * byte: global types by index
  *   - int32: number m of functions in this module
  *   - m * int32: compiled function addresses
  *   - int32: number e of element sections
  *   - e times:
  *     - int32: address of table offset
  *     - int32: number l of table init vector elements
  *     - l * int32: init compiled function addresses
  *   - int32: number d of data sections
  *   - d times:
  *     - int32: address of memory offset
  *     - int32: number k of memory bytes
  *     - k * byte: memory bytes
  *
  *  Little-endian order is used.
  */
private[internals] case class StoredModule(tableMin: Int,
                                           tableMax: Int,
                                           memMin: Int,
                                           memMax: Int,
                                           startAddr: Int,
                                           globTypes: Vector[Byte],
                                           funcAddrs: Vector[Int],
                                           elems: Vector[(Int, Vector[Int])],
                                           data: Vector[(Int, Vector[Byte])])

private[internals] object StoredModule {

  val codec: Codec[StoredModule] =
    (int32L :: int32L :: int32L :: int32L :: int32L :: vectorOfN(int32L, byte) :: vectorOfN(int32L, int32L) :: vectorOfN(
      int32L,
      int32L ~ vectorOfN(int32L, int32L)) :: vectorOfN(int32L, int32L ~ vectorOfN(int32L, byte))).as[StoredModule]

}
