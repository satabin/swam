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
package functions

import formats._

import cats._
import cats.implicits._

import scala.language.higherKinds

class IFunction0Unit[F[_]](f: () => F[Unit])(implicit F: MonadError[F, Throwable])
    extends Function[F] {
  val  tpe = FuncType(Vector(), Vector())
  def invoke(parameters: Vector[Value]): F[Option[Value]] = {
    if (parameters.isEmpty)
      f().map(_ => None)
    else
      F.raiseError(new ConversionException(
        s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
  }
}

class IFunction0[F[_], Ret](f: () => F[Ret])(implicit F: MonadError[F, Throwable], writer: ValueWriter[Ret])
    extends Function[F] {
  val tpe = FuncType(Vector(), Vector(writer.swamType))
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    if (parameters.isEmpty)
      f().map(v => Some(writer.write(v)))
    else
      F.raiseError(new ConversionException(
        s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
}

class IFunction1Unit[F[_], P1](f: (P1) => F[Unit])(implicit F: MonadError[F, Throwable],
                                                            reader1: ValueReader[P1])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType), Vector())
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1) =>
        for {
          p1 <- reader1.read[F](p1)
          _ <- f(p1)
        } yield None
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction1[F[_], P1, Ret](f: (P1) => F[Ret])(implicit F: MonadError[F, Throwable],
                                                            reader1: ValueReader[P1],
                                                            writer: ValueWriter[Ret])
    extends Function[F] {
  val  tpe = FuncType(Vector(reader1.swamType), Vector(writer.swamType))
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1) =>
        for {
          p1 <- reader1.read[F](p1)
          v <- f(p1)
        } yield Some(writer.write(v))
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction2Unit[F[_], P1, P2](f: (P1, P2) => F[Unit])(implicit F: MonadError[F, Throwable],
                                                                    reader1: ValueReader[P1],
                                                                    reader2: ValueReader[P2])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType), Vector())
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2) =>
        for {
          p1 <- reader1.read[F](p1)
          p2 <- reader2.read[F](p2)
          _ <- f(p1, p2)
        } yield None
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction2[F[_], P1, P2, Ret](f: (P1, P2) => F[Ret])(implicit F: MonadError[F, Throwable],
                                                                    reader1: ValueReader[P1],
                                                                    reader2: ValueReader[P2],
                                                                    writer: ValueWriter[Ret])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType), Vector(writer.swamType))
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2) =>
        for {
          p1 <- reader1.read[F](p1)
          p2 <- reader2.read[F](p2)
          v <- f(p1, p2)
        } yield Some(writer.write(v))
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction3Unit[F[_], P1, P2, P3](f: (P1, P2, P3) => F[Unit])(implicit F: MonadError[F, Throwable],
                                                                            reader1: ValueReader[P1],
                                                                            reader2: ValueReader[P2],
                                                                            reader3: ValueReader[P3])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType, reader3.swamType), Vector())
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3) =>
        for {
          p1 <- reader1.read[F](p1)
          p2 <- reader2.read[F](p2)
          p3 <- reader3.read[F](p3)
          _ <- f(p1, p2, p3)
        } yield None
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction3[F[_], P1, P2, P3, Ret](f: (P1, P2, P3) => F[Ret])(implicit F: MonadError[F, Throwable],
                                                                            reader1: ValueReader[P1],
                                                                            reader2: ValueReader[P2],
                                                                            reader3: ValueReader[P3],
                                                                            writer: ValueWriter[Ret])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType, reader3.swamType), Vector(writer.swamType))
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3) =>
        for {
          p1 <- reader1.read[F](p1)
          p2 <- reader2.read[F](p2)
          p3 <- reader3.read[F](p3)
          v <- f(p1, p2, p3)
        } yield Some(writer.write(v))
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction4Unit[F[_], P1, P2, P3, P4](f: (P1, P2, P3, P4) => F[Unit])(
    implicit F: MonadError[F, Throwable],
    reader1: ValueReader[P1],
    reader2: ValueReader[P2],
    reader3: ValueReader[P3],
    reader4: ValueReader[P4])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType, reader3.swamType, reader4.swamType), Vector())
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3, p4) =>
        for {
          p1 <- reader1.read[F](p1)
          p2 <- reader2.read[F](p2)
          p3 <- reader3.read[F](p3)
          p4 <- reader4.read[F](p4)
          _ <- f(p1, p2, p3, p4)
        } yield None
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction4[F[_], P1, P2, P3, P4, Ret](f: (P1, P2, P3, P4) => F[Ret])(
    implicit F: MonadError[F, Throwable],
    reader1: ValueReader[P1],
    reader2: ValueReader[P2],
    reader3: ValueReader[P3],
    reader4: ValueReader[P4],
    writer: ValueWriter[Ret])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType, reader3.swamType, reader4.swamType),
                                   Vector(writer.swamType))
  def invoke(parameters: Vector[Value]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3, p4) =>
        for {
          p1 <- reader1.read[F](p1)
          p2 <- reader2.read[F](p2)
          p3 <- reader3.read[F](p3)
          p4 <- reader4.read[F](p4)
          v <- f(p1, p2, p3, p4)
        } yield Some(writer.write(v))
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}
