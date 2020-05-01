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

import cats.effect._

import java.nio.ByteBuffer

package object test {

  import imports._

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  def table = new Table[IO] {
    val a = Array.ofDim[Function[IO]](10)
    def apply(i: Int) = a(i)
    def size = 10
    def update(i: Int, f: Function[IO]) =
      a(i) = f
    def tpe = TableType(ElemType.FuncRef, Limits(10, Some(20)))
  }

  def buffer = {
    val buffer = ByteBuffer.allocate(2 * pageSize)
    buffer.limit(pageSize)
    buffer
  }

  def printi32(i: Int): IO[Unit] =
    IO(println(i))

  def printi32f32(i: Int, f: Float): IO[Unit] =
    IO(println(s"$i $f"))

  def printi64(l: Long): IO[Unit] =
    IO(println(l))

  def printf32(f: Float): IO[Unit] =
    IO(println(f))

  def printf64(d: Double): IO[Unit] =
    IO(println(d))

  def printf64f64(d1: Double, d2: Double): IO[Unit] =
    IO(println(s"$d1 $d2"))

  def print(): IO[Unit] =
    IO(println("print"))

  def spectestlib =
    Imports[IO](
      TCMap[String, AsIsIO]("spectest" -> TCMap[String, AsIIO](
        "memory" -> buffer,
        "global_i32" -> 666,
        "global_i64" -> 'a'.toLong,
        "global_f32" -> 0.0f,
        "global_f64" -> 0.0d,
        "table" -> table,
        "print_i32" -> printi32 _,
        "print_i64" -> printi64 _,
        "print_f32" -> printf32 _,
        "print_f64" -> printf64 _,
        "print_i32_f32" -> printi32f32 _,
        "print_f64_f64" -> printf64f64 _,
        "print" -> print _
      )))

}
