package swam
package code_analysis
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener, InstructionWrapper}
import java.io._

import cats.implicits._
import cats._
import cats.effect._
import scodec.bits.BitVector
import swam.syntax.{Br, Call, Expr, FuncBody, Import, Inst, Section, i32}
import fs2._
import swam.code_analysis.coverage.utils.TransformationContext

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
class CoverageListener[F[_]: Async](wasi: Boolean) extends InstructionListener[F] {
  var coverageMap = Map[Int, (String, Int)]()

  override val wasiCheck: Boolean = wasi

  var pathInfo: Array[Byte] = new Array[Byte](65536) // TODO pass this as a parameter
  var previousInstId: Int = -1
  var uniqueIds: Set[Int] = Set()

  def clean(): Unit = {
    pathInfo = new Array[Byte](65536)
    previousInstId = -1
  }

  override def before(inner: InstructionWrapper[F], frame: Frame[F]): Unit = {}

  override def after(inner: InstructionWrapper[F],
                     frame: Frame[F],
                     functionName: Option[String],
                     result: Continuation[F]): Continuation[F] = {
    val fn: String = functionName.getOrElse("N/A").toString
    val count: (String, Int) = coverageMap.getOrElse(inner.id, (fn, 0))
    coverageMap = coverageMap.updated(inner.id, (fn, count._2 + 1))

    uniqueIds += inner.id
    if (previousInstId != -1) {
      val index = (previousInstId ^ inner.id)
      pathInfo(index) = (pathInfo(index) + 1).toByte
      previousInstId = inner.id >> 1
    } else
      previousInstId = inner.id
    result
  }

  override def init(inner: InstructionWrapper[F], functionName: Option[String]): Unit = {
    val fn = functionName.getOrElse("N/A").toString
    coverageMap = coverageMap.updated(inner.id, (fn, 0))
  }

  private def getBBLeaders(code: Expr): Set[Int] = {

    var leaders = Set[Int]()
    leaders += 0 // first instruction is a leader

    def getBBSaux(idx: Int): Unit = {

      if (idx < code.length) {

        val inst: Inst = code(idx)

        inst match {
          // Every jump instruction target and the the next instruction is then a leader

          case _ => {}
        }
        getBBSaux(idx + 1)
      }
    }

    getBBSaux(1)
    leaders
  }

  def instrument(sections: Stream[F, Section]): Stream[F, Section] = {

    sections
      .fold(TransformationContext(Seq(), None, None)) {
        case (ctx, c: Section.Types) => ctx.copy(sections = ctx.sections, types = Option(c), imported = ctx.imported)
        /* case (ctx, c: Section.Functions) =>
          ctx.copy(sections = ctx.sections
                     :+ Section.Functions(c.functions :+ c.functions.size),
                   types = ctx.types)*/
        case (ctx, c: Section.Code) =>
          ctx.copy(
            sections = ctx.sections :+
              Section.Code(c.bodies
                .map { x: FuncBody =>
                  FuncBody(x.locals, x.code.map {
                    case Call(funcidx) =>
                      if (funcidx >= ctx.numberOfImportedFunctions) Call(funcidx + 1) else Call(funcidx)
                    case x => x
                  })
                }),
            types = ctx.types,
            imported = ctx.imported
          )
        case (ctx, c: Section.Imports) => {
          ctx.copy(sections = ctx.sections,
                   types = ctx.types,
                   imported =
                     Option(Section.Imports(c.imports.appended(Import.Function("env", "swam_cb", ctx.tpeIndex)))))
        }
        case (ctx, c: Section) => ctx.copy(sections = ctx.sections :+ c, types = ctx.types, imported = ctx.imported)
      }
      .flatMap(t => {
        Stream.emits(t.redefinedTypes +: (t.imported.get +: t.sections))
      })

  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](wasi: Boolean): CoverageListener[F] = {

    new CoverageListener[F](wasi)

  }
}
