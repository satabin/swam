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

  val blockType: Codec[BlockType] =
    fallback(
      varint33,
      mappedEnum(
        byte,
        Map[BlockType, Byte](
          BlockType.ValueType(ValType.I32) -> 0x7f,
          BlockType.ValueType(ValType.I64) -> 0x7e,
          BlockType.ValueType(ValType.F32) -> 0x7d,
          BlockType.ValueType(ValType.F64) -> 0x7c,
          BlockType.NoType -> 0x40
        )
      )
    ).xmap[BlockType]({
      case Left(i)  => BlockType.FunctionType(i)
      case Right(t) => t
    }, {
      case BlockType.FunctionType(idx) => Left(idx)
      case t                           => Right(t)
    })

  val elemType: Codec[ElemType] =
    mappedEnum(byte, Map[ElemType, Byte](ElemType.FuncRef -> 0x70))

  val funcType: Codec[FuncType] =
    mappedEnum(byte, Map[Unit, Byte](() -> 0x60)) ~>
      (("parameters" | vectorOfN(varuint32, valType)) ::
        ("return" | vectorOfN(varuint32, valType))).as[FuncType]

  val globalType: Codec[GlobalType] =
    (("content_type" | valType) ::
      ("mutability" | mappedEnum(byte, Map[Mut, Byte](Mut.Const -> 0x00, Mut.Var -> 0x01)))).as[GlobalType]

  val limits: Codec[Limits] =
    discriminated[Limits]
      .by(byte)
      .|(0x00) { case Limits(min, None) => min }(Limits(_, None))(varuint32)
      .|(0x01) { case Limits(min, Some(max)) => (min, max) }(Limits.tupled)(varuint32 ~ varuint32)

  val tableType: Codec[TableType] =
    (("element_type" | elemType) ::
      ("limits" | limits)).as[TableType]

  val memoryType: Codec[MemType] =
    ("limits" | limits).as[MemType]
}
