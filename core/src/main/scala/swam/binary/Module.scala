package swam
package binary

import syntax._

import scodec.bits._
import scodec.stream._

import scodec._
import scodec.codecs._
import scodec.codecs.literals._

import cats.effect._

import fs2._

import scala.language.higherKinds

import java.io.FileInputStream

object Module {

  private val header: Codec[Unit] =
    ("magic" | hex"0061736d") ~>
      ("version" | hex"01000000")

  val sections: StreamCodec[Section] =
    StreamCodec.instance(
      encode.once(WasmCodec.section),
      decode.once(WasmCodec.section)).many

  val decoder: StreamDecoder[Section] =
    for {
      () <- decode.once(header)
      section <- sections
    } yield section

  def encoder: StreamEncoder[Section] =
    encode.emit(hex"0061736d01000000".bits) ++ sections

}
