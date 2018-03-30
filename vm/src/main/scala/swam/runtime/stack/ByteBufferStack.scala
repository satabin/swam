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

package swam.runtime
package stack

import java.nio.ByteBuffer

class ByteBufferStack(capacity: Int) extends Stack {

  private val stack = ByteBuffer.allocate(capacity)

  private var top = 0

  def push(word: Word): Unit =
    if (top >= stack.capacity) {
      throw new StackOverflowException
    } else {
      stack.putLong(top, word)
      top += 1
    }

  def pop(): Word =
    if (top <= 0) {
      throw new StackUnderflowException
    } else {
      top -= 1
      stack.getLong(top)
    }

}
