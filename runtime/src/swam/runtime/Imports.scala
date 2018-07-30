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

import formats._
import functions._
import internals.instance._

import cats._

import scala.language.higherKinds

private[runtime] trait ImportedModule[F[_]] {

  def find(field: String)(implicit F: MonadError[F, Throwable]): F[ImportableInstance[F]]

}

private[runtime] case class ScalaModule[F[_]](val fields: Map[String, ScalaImport[F]]) extends ImportedModule[F] {

  def find(field: String)(implicit F: MonadError[F, Throwable]): F[ImportableInstance[F]] = fields.get(field) match {
    case Some(f) => F.pure(f)
    case None    => F.raiseError(new RuntimeException(s"Unknown field $field"))
  }

}

private[runtime] sealed trait ScalaImport[F[_]] extends ImportableInstance[F]

private[runtime] class ScalaFunction[F[_]](val f: Importable[F]) extends ScalaImport[F] with HostFunction[F] {
  val tpe = f.tpe

  def invoke(parameters: Seq[Value]): F[Option[Value]] =
    f.apply(parameters: _*)

}

private[runtime] class ScalaVar[T, F[_]](var value: Value)(implicit format: ValueFormatter[T],
                                                           F: MonadError[F, Throwable])
    extends ScalaImport[F]
    with GlobalInstance[F] {

  val tpe: GlobalType =
    GlobalType(format.swamType, Mut.Var)

  def apply(): F[T] =
    format.read(value)

  def update(v: T): Unit =
    value = format.write(v)

  def :=(v: T): Unit =
    value = format.write(v)

}
