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

import java.lang.{Float => JFloat, Double => JDouble}

object F64 {

  def trunc(d: Double): Double =
    if (d >= 0.0d)
      d.floor
    else
      d.ceil

  def nearest(d: Double): Double =
    if (d.isInfinite || d.isNaN || d.isWhole)
      d
    else
      Math.copySign((Math.round(d / 2) * 2).toDouble, d)

  def promote(f: Float): Double =
    if (!f.isNaN) {
      f.toDouble
    } else {
      val nan32bits = JFloat.floatToRawIntBits(f) & 0X00000000FFFFFFFFL
      val signField = (nan32bits >>> 31) << 63
      val significandField = (nan32bits << 41) >>> 12
      val fields = signField | significandField
      val nan64bits = 0X7FF8000000000000L | fields
      JDouble.longBitsToDouble(nan64bits)
    }

  def convertSi32(i: Int): Double =
    i.toDouble

  def convertUi32(i: Int): Double =
    (i & 0X00000000FFFFFFFFL).toDouble

  def convertSi64(l: Long): Double =
    l.toDouble

  def convertUi64(l: Long): Double =
    if (l >= 0L)
      l.toDouble
    else
      ((l >>> 1) | (l & 1L)) * 2.0d

  def reinterpret(l: Long): Double =
    JDouble.longBitsToDouble(l)

}
