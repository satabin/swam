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
package imports

import formats._

import cats._
import cats.implicits._

class IFunction2Unit[F[_], P1, P2](f: (P1, P2) => F[Unit])(implicit reader1: ValueReader[F, P1],
                                                           reader2: ValueReader[F, P2],
                                                           F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType), Vector())
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          _ <- f(p1, p2)
        } yield None
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction2[F[_], P1, P2, Ret](f: (P1, P2) => F[Ret])(implicit reader1: ValueReader[F, P1],
                                                           reader2: ValueReader[F, P2],
                                                           writer: ValueWriter[F, Ret],
                                                           F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType), Vector(writer.swamType))
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          v <- f(p1, p2)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction3[F[_], P1, P2, P3, Ret](f: (P1, P2, P3) => F[Ret])(implicit reader1: ValueReader[F, P1],
                                                                   reader2: ValueReader[F, P2],
                                                                   reader3: ValueReader[F, P3],
                                                                   writer: ValueWriter[F, Ret],
                                                                   F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe = FuncType(Vector(reader1.swamType, reader2.swamType, reader3.swamType), Vector(writer.swamType))
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          p3 <- reader3.read(p3, m)
          v <- f(p1, p2, p3)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction4[F[_], P1, P2, P3, P4, Ret](f: (P1, P2, P3, P4) => F[Ret])(implicit reader1: ValueReader[F, P1],
                                                                           reader2: ValueReader[F, P2],
                                                                           reader3: ValueReader[F, P3],
                                                                           reader4: ValueReader[F, P4],
                                                                           writer: ValueWriter[F, Ret],
                                                                           F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe =
    FuncType(Vector(reader1.swamType, reader2.swamType, reader3.swamType, reader4.swamType), Vector(writer.swamType))
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3, p4) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          p3 <- reader3.read(p3, m)
          p4 <- reader4.read(p4, m)
          v <- f(p1, p2, p3, p4)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction5[F[_], P1, P2, P3, P4, P5, Ret](f: (P1, P2, P3, P4, P5) => F[Ret])(implicit reader1: ValueReader[F, P1],
                                                                                   reader2: ValueReader[F, P2],
                                                                                   reader3: ValueReader[F, P3],
                                                                                   reader4: ValueReader[F, P4],
                                                                                   reader5: ValueReader[F, P5],
                                                                                   writer: ValueWriter[F, Ret],
                                                                                   F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe =
    FuncType(Vector(reader1.swamType, reader2.swamType, reader3.swamType, reader4.swamType, reader5.swamType),
             Vector(writer.swamType))
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3, p4, p5) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          p3 <- reader3.read(p3, m)
          p4 <- reader4.read(p4, m)
          p5 <- reader5.read(p5, m)
          v <- f(p1, p2, p3, p4, p5)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction6[F[_], P1, P2, P3, P4, P5, P6, Ret](f: (P1, P2, P3, P4, P5, P6) => F[Ret])(
    implicit reader1: ValueReader[F, P1],
    reader2: ValueReader[F, P2],
    reader3: ValueReader[F, P3],
    reader4: ValueReader[F, P4],
    reader5: ValueReader[F, P5],
    reader6: ValueReader[F, P6],
    writer: ValueWriter[F, Ret],
    F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe =
    FuncType(Vector(reader1.swamType,
                    reader2.swamType,
                    reader3.swamType,
                    reader4.swamType,
                    reader5.swamType,
                    reader6.swamType),
             Vector(writer.swamType))
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3, p4, p5, p6) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          p3 <- reader3.read(p3, m)
          p4 <- reader4.read(p4, m)
          p5 <- reader5.read(p5, m)
          p6 <- reader6.read(p6, m)
          v <- f(p1, p2, p3, p4, p5, p6)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction7[F[_], P1, P2, P3, P4, P5, P6, P7, Ret](f: (P1, P2, P3, P4, P5, P6, P7) => F[Ret])(
    implicit reader1: ValueReader[F, P1],
    reader2: ValueReader[F, P2],
    reader3: ValueReader[F, P3],
    reader4: ValueReader[F, P4],
    reader5: ValueReader[F, P5],
    reader6: ValueReader[F, P6],
    reader7: ValueReader[F, P7],
    writer: ValueWriter[F, Ret],
    F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe =
    FuncType(
      Vector(reader1.swamType,
             reader2.swamType,
             reader3.swamType,
             reader4.swamType,
             reader5.swamType,
             reader6.swamType,
             reader7.swamType),
      Vector(writer.swamType)
    )
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3, p4, p5, p6, p7) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          p3 <- reader3.read(p3, m)
          p4 <- reader4.read(p4, m)
          p5 <- reader5.read(p5, m)
          p6 <- reader6.read(p6, m)
          p7 <- reader7.read(p7, m)
          v <- f(p1, p2, p3, p4, p5, p6, p7)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}

class IFunction8[F[_], P1, P2, P3, P4, P5, P6, P7, P8, Ret](f: (P1, P2, P3, P4, P5, P6, P7, P8) => F[Ret])(
    implicit reader1: ValueReader[F, P1],
    reader2: ValueReader[F, P2],
    reader3: ValueReader[F, P3],
    reader4: ValueReader[F, P4],
    reader5: ValueReader[F, P5],
    reader6: ValueReader[F, P6],
    reader7: ValueReader[F, P7],
    reader8: ValueReader[F, P8],
    writer: ValueWriter[F, Ret],
    F: MonadError[F, Throwable])
    extends Function[F] {
  val tpe =
    FuncType(
      Vector(reader1.swamType,
             reader2.swamType,
             reader3.swamType,
             reader4.swamType,
             reader5.swamType,
             reader6.swamType,
             reader7.swamType,
             reader8.swamType),
      Vector(writer.swamType)
    )
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]] =
    parameters match {
      case Seq(p1, p2, p3, p4, p5, p6, p7, p8) =>
        for {
          p1 <- reader1.read(p1, m)
          p2 <- reader2.read(p2, m)
          p3 <- reader3.read(p3, m)
          p4 <- reader4.read(p4, m)
          p5 <- reader5.read(p5, m)
          p6 <- reader6.read(p6, m)
          p7 <- reader7.read(p7, m)
          p8 <- reader8.read(p8, m)
          v <- f(p1, p2, p3, p4, p5, p6, p7, p8)
          v <- writer.write(v, m)
        } yield Some(v)
      case _ =>
        F.raiseError(new ConversionException(
          s"function expects ${tpe.params.mkString("(", ", ", ")")} but got ${parameters.map(_.tpe).mkString("(", ", ", ")")}"))
    }
}
