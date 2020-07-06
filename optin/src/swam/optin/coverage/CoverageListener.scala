package swam
package optin
package coverage

import java.io.File

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener}
import cats.effect.{Async, ContextShift, IO}
import cats._
import kantan.csv.{HeaderEncoder, rfc}

import kantan.csv._
import kantan.csv.ops._

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
class CoverageListener[F[_]: Async] extends InstructionListener[F] {

  var coverageMap = Map[AsmInst[F], (String, Int)]()

  def buildCoverage(): List[ModuleCoverageInfo] = {

    val covList = coverageMap
      .groupBy {
        case (_, (name, _)) => name
      }
      .toList
      .map {
        case (name, map) => ModuleCoverageInfo(name, map.count(t => t._2._2 > 0), map.size)
      }

    covList
  }

  override def before(inner: AsmInst[F], frame: Frame[F]): Unit = {}

  override def after(inner: AsmInst[F], frame: Frame[F], result: Continuation[F]): Continuation[F] = {
    val prev = coverageMap(inner)
    coverageMap = coverageMap.updated(inner, (prev._1, 1))

    result
  }

  override def init(inner: AsmInst[F], functionName: Option[String]): Unit = {
    coverageMap = coverageMap.updated(inner, (functionName.getOrElse("N/A"), 0))
  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](): CoverageListener[F] = new CoverageListener[F]()
}
