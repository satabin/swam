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

package swam.util.logging

import enumeratum.values._

import cats._
import cats.implicits._

sealed abstract class LogLevel(val value: Int) extends IntEnumEntry

object LogLevel extends IntEnum[LogLevel] {

  val values = findValues

  case object Critical extends LogLevel(0)
  case object Error extends LogLevel(1)
  case object Warning extends LogLevel(2)
  case object Info extends LogLevel(3)
  case object Debug extends LogLevel(4)

  implicit val LogLevelOrder: Order[LogLevel] =
    Order[Int].contramap(_.value)

}
