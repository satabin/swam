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

import java.lang.{Float => JFloat, Double => JDouble, Long => JLong}

object F32 {

  def trunc(f: Float): Float =
    if (f >= 0.0f)
      f.floor
    else
      f.ceil

  def nearest(f: Float): Float =
    if (f.isInfinite || f.isNaN || f.isWhole)
      f
    else
      Math.copySign((Math.round(f / 2) * 2).toFloat, f)

  def demote(d: Double): Float =
    if (!d.isNaN) {
      d.toFloat
    } else {
      val nan64bits = JDouble.doubleToRawLongBits(d)
      val signField = (nan64bits >>> 63) << 31
      val significandField = (nan64bits << 12) >>> 41
      val fields = signField | significandField
      val nan32bits = 0x7fc00000 | fields.toInt
      JFloat.intBitsToFloat(nan32bits)
    }

  def convertSi32(i: Int): Float =
    i.toFloat

  def convertUi32(i: Int): Float =
    if (i >= 0)
      i.toFloat
    else
      ((i >>> 1) | (i & 1)).toFloat * 2.0f

  private val convC = JFloat.parseFloat("0x1p12")

  def convertSi64(l: Long): Float =
    if (math.abs(l) < 0X10000000000000L) {
      l.toFloat
    } else {
      val r = if ((l & 0XFFFL) == 0L) 0L else 1L
      ((l >> 12) | r).toFloat * convC
    }

  def convertUi64(l: Long): Float =
    if (JLong.compareUnsigned(l, 0X10000000000000L) < 0) {
      l.toFloat
    } else {
      val r = if ((l & 0XFFFL) == 0L) 0L else 1L
      ((l >>> 12) | r).toFloat * convC
    }

  def reinterpret(i: Int): Float =
    JFloat.intBitsToFloat(i)

}
