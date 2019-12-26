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
package trace

import enumeratum._

sealed trait EventType extends EnumEntry with EnumEntry.Lowercase

object EventType extends Enum[EventType] {
  case object MSize extends EventType
  case object MRead extends EventType
  case object MWrite extends EventType
  case object MGrow extends EventType
  case object SPush extends EventType
  case object SPop extends EventType
  case object SPeek extends EventType

  def values = findValues
}
