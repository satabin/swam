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

/** This package contains all the classes and types related to
  * running WebAssembly modules.
  *
  * The entry point for almost all users will be [[Engine]].
  * A typical use of the engine is:
  * {{{
  * import swam._
  * import text._
  * import runtime._
  * import exports._
  *
  * import cats.implicits._
  * import cats.effect._
  *
  * import java.nio.file.Paths
  *
  * val tcompiler = Compiler[IO]
  * val engine = Engine[IO]
  *
  * for {
  *   tcompiler <- tcompiler
  *   engine <- engine
  *   mod <- engine.compile(/* source of the module */)
  *   inst <- mod.newInstance()
  *   f <- inst.exports.function1[Int, Int]("f")
  *   res <- f(43)
  * } yield res
  *
  * println(res.unsafeRunSync())
  * }}}
  */
package object runtime {

  /** The size in bytes of a memory page. */
  val pageSize: Int = 65536

  type CanFail[+T] = Either[String, T]

}
