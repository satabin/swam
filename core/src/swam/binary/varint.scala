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

package swam.binary

import scodec._
import scodec.bits._

import scala.annotation.tailrec

private class Varulong(bits: Int) extends Codec[Long] {

  private val bitsL = bits.toLong

  val MaxValue = (1l << bits) - 1
  val MinValue = 0

  private def description = s"$bits-bit unsigned leb128 integer"

  def decode(buffer: BitVector): Attempt[DecodeResult[Long]] =
    if (buffer.sizeLessThan(1))
      Attempt.failure(Err.insufficientBits(1, buffer.size))
    else
      decode(buffer.bytes, 0, 0)

  @tailrec
  private def decode(buffer: ByteVector, result: Long, shift: Int): Attempt[DecodeResult[Long]] =
    if (shift / 7 + 1 > maxBits) {
      Attempt.failure(Err(s"cannot decode values encoded on more than $maxBits bits for $description"))
    } else {
      val byte = buffer.head
      val result1 = result | ((byte & 0x7fl) << shift)
      if ((byte & 0x80) == 0x80)
        decode(buffer.tail, result1, shift + 7)
      else
        Attempt.successful(DecodeResult(result1, buffer.tail.bits))
    }

  def encode(value: Long): Attempt[BitVector] =
    if (value > MaxValue)
      Attempt.failure(Err(s"$value is greater than maximum value $MaxValue for $description"))
    else if (value < MinValue)
      Attempt.failure(Err(s"$value is less than minimum value $MinValue for $description"))
    else
      Attempt.successful(encode(value, ByteVector.empty))

  @tailrec
  private def encode(value: Long, buffer: ByteVector): BitVector = {
    val byte = (value & 0x7f).toByte
    val value1 = value >> 7
    val byte1 =
      if (value1 != 0)
        (byte | 0x80).toByte
      else
        byte
    val buffer1 = buffer :+ byte1
    if (value1 == 0)
      buffer1.bits
    else
      encode(value1, buffer1)
  }

  private val maxBits = (bitsL / 7) + (if (bitsL % 7 == 0) 0 else 1) * 8

  def sizeBound: SizeBound =
    SizeBound.bounded(8l, maxBits)

}

private class Varuint(bits: Int) extends Codec[Int] {

  private val bitsL = bits.toLong

  val MaxValue = (1 << bits) - 1
  val MinValue = 0

  private def description = s"$bits-bit unsigned leb128 integer"

  def decode(buffer: BitVector): Attempt[DecodeResult[Int]] =
    if (buffer.sizeLessThan(1))
      Attempt.failure(Err.insufficientBits(1, buffer.size))
    else
      decode(buffer.bytes, 0, 0)

  @tailrec
  private def decode(buffer: ByteVector, result: Int, shift: Int): Attempt[DecodeResult[Int]] =
    if (shift / 7 + 1 > maxBits) {
      Attempt.failure(Err(s"cannot decode values encoded on more than $maxBits bits for $description"))
    } else {
      val byte = buffer.head
      val result1 = result | ((byte & 0x7f) << shift)
      if ((byte & 0x80) == 0x80)
        decode(buffer.tail, result1, shift + 7)
      else
        Attempt.successful(DecodeResult(result1, buffer.tail.bits))
    }

  def encode(value: Int): Attempt[BitVector] =
    if (value > MaxValue)
      Attempt.failure(Err(s"$value is greater than maximum value $MaxValue for $description"))
    else if (value < MinValue)
      Attempt.failure(Err(s"$value is less than minimum value $MinValue for $description"))
    else
      Attempt.successful(encode(value, ByteVector.empty))

  @tailrec
  private def encode(value: Int, buffer: ByteVector): BitVector = {
    val byte = (value & 0x7f).toByte
    val value1 = value >> 7
    val byte1 =
      if (value1 != 0)
        (byte | 0x80).toByte
      else
        byte
    val buffer1 = buffer :+ byte1
    if (value1 == 0)
      buffer1.bits
    else
      encode(value1, buffer1)
  }

  private val maxBits = (bitsL / 7) + (if (bitsL % 7 == 0) 0 else 1)

  def sizeBound: SizeBound =
    SizeBound.bounded(1l, maxBits)

}

private class Varint(bits: Int) extends Codec[Int] {

  private val bitsL = bits.toLong

  val MaxValue = (1 << (bits - 1)) - 1
  val MinValue = -(1 << (bits - 1))

  private def description = s"$bits-bit signed leb128 integer"

  def decode(buffer: BitVector): Attempt[DecodeResult[Int]] =
    if (buffer.sizeLessThan(1))
      Attempt.failure(Err.insufficientBits(1, buffer.size))
    else
      decode(buffer.bytes, 0, 0)

  @tailrec
  private def decode(buffer: ByteVector, result: Int, shift: Int): Attempt[DecodeResult[Int]] =
    if (shift / 7 + 1 > maxBits) {
      Attempt.failure(Err(s"cannot decode values encoded on more than $maxBits bits for $description"))
    } else {
      val byte = buffer.head
      val result1 = result | ((byte & 0x7f) << shift)
      val shift1 = shift + 7
      if ((byte & 0x80) == 0x80)
        decode(buffer.tail, result1, shift1)
      else if (shift1 < bits && (byte & 0x40) == 0x40)
        Attempt.successful(DecodeResult(result1 | (~0 << shift1), buffer.tail.bits))
      else
        Attempt.successful(DecodeResult(result1, buffer.tail.bits))
    }

  def encode(value: Int): Attempt[BitVector] =
    if (value > MaxValue)
      Attempt.failure(Err(s"$value is greater than maximum value $MaxValue for $description"))
    else if (value < MinValue)
      Attempt.failure(Err(s"$value is less than minimum value $MinValue for $description"))
    else
      Attempt.successful(encode(value, true, ByteVector.empty))

  @tailrec
  private def encode(value: Int, more: Boolean, buffer: ByteVector): BitVector =
    if (more) {
      val byte = (value & 0x7f).toByte
      val value1 = value >> 7

      val (byte1, more1) =
        if ((value1 == 0 && (byte & 0x40) == 0x00) || (value1 == -1 && (byte & 0x40) == 0x40))
          (byte, false)
        else
          ((byte | 0x80).toByte, true)
      val buffer1 = buffer :+ byte1
      encode(value1, more1, buffer1)
    } else {
      buffer.bits
    }

  private val maxBits = (bitsL / 7) + (if (bitsL % 7 == 0) 0 else 1)

  def sizeBound: SizeBound =
    SizeBound.bounded(1l, maxBits)

}

private class Varlong(bits: Int) extends Codec[Long] {

  private val bitsL = bits.toLong

  val MaxValue = (1l << (bits - 1)) - 1
  val MinValue = -(1l << (bits - 1))

  private def description = s"$bits-bit signed leb128 integer"

  def decode(buffer: BitVector): Attempt[DecodeResult[Long]] =
    if (buffer.sizeLessThan(1))
      Attempt.failure(Err.insufficientBits(1, buffer.size))
    else
      decode(buffer.bytes, 0, 0)

  @tailrec
  private def decode(buffer: ByteVector, result: Long, shift: Int): Attempt[DecodeResult[Long]] =
    if (shift / 7 + 1 > maxBits) {
      Attempt.failure(Err(s"cannot decode values encoded on more than $maxBits bits for $description"))
    } else {
      val byte = buffer.head
      val result1 = result | ((byte & 0x7fl) << shift)
      val shift1 = shift + 7
      if ((byte & 0x80) == 0x80)
        decode(buffer.tail, result1, shift1)
      else if (shift1 < bits && (byte & 0x40) == 0x40)
        Attempt.successful(DecodeResult(result1 | (~0 << shift1), buffer.tail.bits))
      else
        Attempt.successful(DecodeResult(result1, buffer.tail.bits))
    }

  def encode(value: Long): Attempt[BitVector] =
    if (value > MaxValue)
      Attempt.failure(Err(s"$value is greater than maximum value $MaxValue for $description"))
    else if (value < MinValue)
      Attempt.failure(Err(s"$value is less than minimum value $MinValue for $description"))
    else
      Attempt.successful(encode(value, true, ByteVector.empty))

  @tailrec
  private def encode(value: Long, more: Boolean, buffer: ByteVector): BitVector =
    if (more) {
      val byte = (value & 0x7f).toByte
      val value1 = value >> 7

      val (byte1, more1) =
        if ((value1 == 0 && (byte & 0x40) == 0x00) || (value1 == -1 && (byte & 0x40) == 0x40))
          (byte, false)
        else
          ((byte | 0x80).toByte, true)
      val buffer1 = buffer :+ byte1
      encode(value1, more1, buffer1)
    } else {
      buffer.bits
    }

  private val maxBits = (bitsL / 7) + (if (bitsL % 7 == 0) 0 else 1)

  def sizeBound: SizeBound =
    SizeBound.bounded(1l, maxBits)

}
