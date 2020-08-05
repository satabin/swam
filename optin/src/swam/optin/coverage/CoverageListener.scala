package swam
package optin
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener}
import cats.effect.{Async, ContextShift}
import cats._

import java.io._


/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
class CoverageListener[F[_]: Async](wasi:Boolean, filterFunc:String, covr: Boolean, covs: Boolean) extends InstructionListener[F] {
  //(//Index //Function name) -> (Functions, Actual instruction, HitCount)
  var coverageMap = Map[(Int, String), (String, AsmInst[F], Int)]()

  //(Instruction Index, Function Name -> FunctionName, MapOfPath[(current Block id, previous Block id ), hitCount])
  //var coveragePath = Map[(Int, String), (String, AsmInst[F], Int, Int, Int)]()

  override val wasiCheck : Boolean = wasi

  override val covreport: Boolean = covr

  override val covshowmap: Boolean = covs

  override val filter : String = filterFunc

  override def before(inner: AsmInst[F], index: Int, functionName: Option[String],frame: Frame[F]): Unit = {}

  override def after(inner: AsmInst[F], index: Int, frame: Frame[F], functionName:Option[String],result: Continuation[F]): Continuation[F] = {
    val fn:String = functionName.getOrElse("N/A").toString
    val prev = coverageMap((index,fn))
    val count:Option[(String,AsmInst[F],Int)] = coverageMap.get((index, fn))
    if (count == null) {
      coverageMap = coverageMap.updated((index, fn), (fn, prev._2, 1))
    }
    else{
      val i = (count match {
        case Some(x) => x._3 + 1 // this extracts the value in a as an Int
        case _ => 1
        })
      coverageMap = coverageMap.updated((index, fn), (fn, prev._2, i))
    }
    //println(coverageMap)
    result
  }

  override def init(inner: AsmInst[F], index: Int, functionName: Option[String]): Unit = {
    val fn = functionName.getOrElse("N/A").toString
    coverageMap = coverageMap.updated((index, fn), (fn, inner, 0))
  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](wasi:Boolean, filterFunc: String, 
    covreport: Boolean, covshowmap: Boolean): CoverageListener[F] = {

    new CoverageListener[F](wasi, filterFunc, covreport, covshowmap)

  }
}
