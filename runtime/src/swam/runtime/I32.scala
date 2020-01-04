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

import java.lang.{Float => JFloat}

object I32 {

  def wrap(l: Long): Int =
    (l % (1L << 32)).toInt

  def truncSf32(f: Float): CanFail[Int] =
    if (f.isNaN)
      Left("invalid conversion to integer")
    else if (f >= -Int.MinValue.toFloat || f < Int.MinValue.toFloat)
      Left("integer overflow")
    else
      Right(f.toInt)

  def truncUf32(f: Float): CanFail[Int] =
    if (f.isNaN)
      Left("invalid conversion to integer")
    else if (f >= -Int.MinValue.toFloat * 2.0f || f <= -1.0f)
      Left("integer overflow")
    else
      Right(f.toLong.toInt)

  def truncSf64(d: Double): CanFail[Int] =
    if (d.isNaN)
      Left("invalid conversion to integer")
    else if (d >= -Int.MinValue.toDouble || d < Int.MinValue)
      Left("integer overflow")
    else
      Right(d.toInt)

  def truncUf64(d: Double): CanFail[Int] =
    if (d.isNaN)
      Left("invalid conversion to integer")
    else if (d >= -Int.MinValue.toDouble * 2.0d || d <= -1.0)
      Left("integer overflow")
    else
      Right(d.toLong.toInt)

  def reinterpret(f: Float): Int =
    JFloat.floatToRawIntBits(f)

}
