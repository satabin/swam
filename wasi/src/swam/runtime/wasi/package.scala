/*
 * Copyright 2020 Lucas Satabin
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

import java.nio.file.OpenOption

package object wasi {

  type Pointer = Int

  type Size = Int

  type Filesize = Long

  type Timestamp = Long

  type Fd = Int

  type Rights = Long

  type FdFlags = Short

  type Lookupflags = Int

  type Oflags = Short

  type Fstflags = Short

  type Filedelta = Long

  type Exitcode = Int

  type Riflags = Short

  type Roflags = Short

  type Siflags = Short

  type Sdflags = Byte

  type Dircookie = Long

  implicit class ShortFlagsOps(val flags: Short) extends AnyVal {
    def when(contains: Short, option: OpenOption, dflt: Option[OpenOption] = None): Option[OpenOption] =
      if ((flags & contains) == contains)
        Some(option)
      else
        dflt
  }

  implicit class LongFlagsOps(val flags: Long) extends AnyVal {
    def whenOneOf(contains: Long, option: OpenOption, dflt: Option[OpenOption] = None): Option[OpenOption] =
      if ((flags & contains) != 0L)
        Some(option)
      else
        dflt
  }

}
