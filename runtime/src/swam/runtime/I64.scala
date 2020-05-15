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

import java.lang.{Double => JDouble}

object I64 {

  def extendSi32(i: Int): Long =
    i.toLong

  def extendUi32(i: Int): Long =
    i & 0X00000000FFFFFFFFL

  def extendS(width: Int, l: Long): Long = {
    val shift = 64 - width
    (l << shift) >> shift
  }

  def truncSf32(f: Float): CanFail[Long] =
    if (f.isNaN)
      Left("invalid conversion to integer")
    else if (f >= -Long.MinValue.toFloat || f < Long.MinValue.toFloat)
      Left("integer overflow")
    else
      Right(f.toLong)

  def truncSatSf32(f: Float): Long =
    if (f.isNaN)
      0
    else if (f >= -Long.MinValue.toFloat)
      Long.MaxValue
    else if (f < Long.MinValue.toFloat)
      Long.MinValue
    else
      f.toLong

  def truncUf32(f: Float): CanFail[Long] =
    if (f.isNaN)
      Left("invalid conversion to integer")
    else if (f >= -Long.MinValue.toDouble * 2.0d || f <= -1.0d)
      Left("integer overflow")
    else if (f >= -Long.MinValue.toDouble)
      Right((f - 9223372036854775808.0f).toLong | Long.MinValue)
    else
      Right(f.toLong)

  def truncSatUf32(f: Float): Long =
    if (f.isNaN)
      0
    else if (f >= -Long.MinValue.toDouble * 2.0d)
      -1
    else if (f <= -1.0d)
      0
    else if (f >= -Long.MinValue.toDouble)
      (f - 9223372036854775808.0f).toLong | Long.MinValue
    else
      f.toLong

  def truncSf64(d: Double): CanFail[Long] =
    if (d.isNaN)
      Left("invalid conversion to integer")
    else if (d >= -Long.MinValue.toDouble || d < Long.MinValue.toDouble)
      Left("integer overflow")
    else
      Right(d.toLong)

  def truncSatSf64(d: Double): Long =
    if (d.isNaN)
      0
    else if (d >= -Long.MinValue.toDouble)
      Long.MaxValue
    else if (d < Long.MinValue.toDouble)
      Long.MinValue
    else
      d.toLong

  def truncUf64(d: Double): CanFail[Long] =
    if (d.isNaN)
      Left("invalid conversion to integer")
    else if (d >= -Long.MinValue.toDouble * 2.0d || d <= -1.0d)
      Left("integer overflow")
    else if (d >= -Long.MinValue.toDouble)
      Right((d - 9223372036854775808.0d).toLong | Long.MinValue)
    else
      Right(d.toLong)

  def truncSatUf64(d: Double): Long =
    if (d.isNaN)
      0
    else if (d >= -Long.MinValue.toDouble * 2.0d)
      -1
    else if (d < 0.0d)
      0
    else if (d >= -Long.MinValue.toDouble)
      (d - 9223372036854775808.0d).toLong | Long.MinValue
    else
      d.toLong

  def reinterpret(d: Double): Long =
    JDouble.doubleToRawLongBits(d)

}
