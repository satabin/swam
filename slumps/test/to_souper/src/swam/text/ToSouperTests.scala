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
import binary._
import validation._



import utest._

//import fastparse._

import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import slumps.config._


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


  val conf: SlumpsConfiguration = new SlumpsConfiguration("")
  val validator = Validator[IO].unsafeRunSync();
  val slumps = Slumps[IO](conf, validator);

  def run(wast: String) = {


  }

  val tests = Tests{
    "console_tracer" - "slumps/tests/resources/slumpt/primes-nockeck.wat"
  }
  
}
