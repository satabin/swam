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
package binary

import scodec._
import scodec.codecs._

trait TypeCodec {

  val valType: Codec[ValType] =
    mappedEnum(byte,
               Map[ValType, Byte](ValType.I32 -> 0x7f, ValType.I64 -> 0x7e, ValType.F32 -> 0x7d, ValType.F64 -> 0x7c))

  val blockType: Codec[Option[ValType]] =
    mappedEnum(byte,
               Map[Option[ValType], Byte](Some(ValType.I32) -> 0x7f,
                                          Some(ValType.I64) -> 0x7e,
                                          Some(ValType.F32) -> 0x7d,
                                          Some(ValType.F64) -> 0x7c,
                                          None -> 0x40))

  val elemType: Codec[ElemType] =
    mappedEnum(byte, Map[ElemType, Byte](ElemType.AnyFunc -> 0x70))

  val funcType: Codec[FuncType] =
    mappedEnum(byte, Map[Unit, Byte](() -> 0x60)) ~>
      (("parameters" | vectorOfN(varuint32, valType)) ::
        ("return" | vectorOfN(varuint1, valType))).as[FuncType]

  val globalType: Codec[GlobalType] =
    (("content_type" | valType) ::
      ("mutability" | mappedEnum(byte, Map[Mut, Byte](Mut.Const -> 0x00, Mut.Var -> 0x01)))).as[GlobalType]

  val limits: Codec[Limits] =
    discriminated[Limits]
      .by(varuint1)
      .|(0) { case Limits(min, None) => min }(Limits(_, None))(varuint32)
      .|(1) { case Limits(min, Some(max)) => (min, max) }(Limits.tupled)(varuint32 ~ varuint32)

  val tableType: Codec[TableType] =
    (("element_type" | elemType) ::
      ("limits" | limits)).as[TableType]

  val memoryType: Codec[MemType] =
    ("limits" | limits).as[MemType]
}
