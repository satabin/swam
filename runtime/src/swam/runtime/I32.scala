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

import java.lang.{Float=>JFloat}

object I32 {

  def wrap(l: Long): Int =
    (l % (1l << 32)).toInt

  def truncSf32(f: Float): Int =
    if(f.isNaN)
      throw new ConversionException("invalid signed i32")
    else if(f >= -Int.MinValue.toFloat || f < Int.MinValue.toFloat)
      throw new ConversionException("invalid signed i32")
    else
      f.toInt

  def truncUf32(f: Float): Int =
    if(f.isNaN)
      throw new ConversionException("invalid unsigned i32")
    else if(f >= -Int.MinValue.toFloat * 2.0f || f <= -1.0f)
      throw new ConversionException("invalid unsigned i32")
    else
      f.toLong.toInt

  def truncSf64(d: Double): Int =
    if(d.isNaN)
      throw new ConversionException("invalid signed i32")
    else if(d >= -Int.MinValue.toDouble || d < Int.MinValue)
      throw new ConversionException("invalid signed i32")
    else
      d.toInt

  def truncUf64(d: Double): Int =
    if(d.isNaN)
      throw new ConversionException("invalid unsigned i32")
    else if(d >= -Int.MinValue.toDouble * 2.0d || d <= -1.0)
      throw new ConversionException("invalid unsigned i32")
    else
      d.toLong.toInt

  def reinterpret(f: Float): Int =
    JFloat.floatToRawIntBits(f)

}
