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
package text

import slumps._
import config._
import runtime._
import binary._
import validation._


import swam.test.util._

import utest._

import better.files._

import fastparse._

import cats._
import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext
import java.nio.file.Paths
import fs2._
import scala.language.higherKinds



object ModuleParser {
  def apply[F[_]](implicit F: Effect[F]): F[ModuleParser[F]] =
    for {
      validator <- Validator[F]
    } yield ModuleParser[F](validator)

  def apply[F[_]](validator: Validator[F])(implicit F: Effect[F]): ModuleParser[F] =
    new ModuleParser[F](validator)
}


object ToSouperTests extends TestSuite {

  
  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)


  def run(wast: String) = {

    val parser = ModuleParser[IO]
      
  }

  val tests = Tests{
    "console_tracer" - "slumps/tests/resources/slumpt/primes-nockeck.wat"
  }
  
}
