package swam
package code_analysis
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener, InstructionWrapper}
import java.io._

import cats.implicits._
import cats._
import cats.effect._
import scodec.bits.BitVector
import swam.syntax.{
  Block,
  Br,
  BrIf,
  Call,
  CallIndirect,
  Expr,
  FuncBody,
  If,
  Import,
  Inst,
  Loop,
  MemorySize,
  Section,
  i32
}
import fs2._
import scodec.Attempt
import swam.binary.custom.NameSectionHandler
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

  val ran = scala.util.Random
  var blockCount = 0
  var instructionCount = 0

  def instrumentVector(instr: Vector[Inst], ctx: TransformationContext): Vector[Inst] = {

    instr.zipWithIndex.flatMap {
      case (CallIndirect(funcidx), i) => {
        instructionCount += 1
        Vector(CallIndirect(funcidx))
      }
      case (Call(funcidx), i) =>
        if (funcidx >= ctx.cbFuncIndex) {
          instructionCount += 1
          Vector(
            Call(funcidx + 1)
          ) // Increment the call index for collision between previous and the new injected cb function
        } else {
          instructionCount += 1
          Vector(Call(funcidx))
        }
      case (Block(tpe, instr), i) => {
        instructionCount += 1
        Vector(Block(tpe, instrumentVector(instr, ctx)))
      }
      case (Loop(tpe, instr), i) => {
        instructionCount += 1
        Vector(Loop(tpe, instrumentVector(instr, ctx)))
      }
      case (If(tpe, thn, els), i) => {
        instructionCount += 1
        Vector(If(tpe, instrumentVector(thn, ctx), instrumentVector(els, ctx)))
      }
      case (BrIf(lbl), i) => {
        instructionCount += 1
        blockCount += 1
        Vector(BrIf(lbl), i32.Const(ran.nextInt(Int.MaxValue)), Call(ctx.cbFuncIndex))
      }
      case (x, i) => {
        instructionCount += 1
        if (i == 0) {
          blockCount += 1
          Vector(i32.Const(ran.nextInt(Int.MaxValue)), Call(ctx.cbFuncIndex), x)
        } else
          Vector(x)
      }

    }
  }

  def instrument(sections: Stream[F, Section]): Stream[F, Section] = {

    val r = for {
      firstPass <- sections
        .fold(TransformationContext(Seq(), None, None, None)) {
          case (ctx, c: Section.Types) =>
            ctx.copy(sections = ctx.sections, types = Option(c), imported = ctx.imported, code = ctx.code)
          case (ctx, c: Section.Custom) => // Patch removing custom section
            ctx.copy(sections = ctx.sections, types = ctx.types, imported = ctx.imported, code = ctx.code)
          case (ctx, c: Section.Imports) => {
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              code = ctx.code,
              //imported = Option(c)
              imported = Option(Section.Imports(c.imports.appended(Import.Function("swam", "swam_cb", ctx.tpeIndex))))
            )
          }
          case (ctx, c: Section.Code) => {
            ctx.copy(sections = ctx.sections, types = ctx.types, imported = ctx.imported, code = Option(c))
          }
          case (ctx, c: Section) =>
            ctx.copy(sections = ctx.sections :+ c, types = ctx.types, imported = ctx.imported, code = ctx.code)
        }

      ctx = firstPass.copy(
        sections = firstPass.sections,
        types = firstPass.types,
        code = firstPass.code,
        imported =
          if (firstPass.imported.isEmpty)
            Option(Section.Imports(Vector(Import.Function("env", "swam_cb", firstPass.tpeIndex))))
          else
            firstPass.imported
      )
      wrappingCode = ctx.copy(
        sections = ctx.sections,
        types = ctx.types,
        code = Option(
          Section.Code(
            firstPass.code.get.bodies
              .map(f => FuncBody(f.locals, instrumentVector(f.code, ctx)))
          )
        ),
        imported = ctx.imported
      )

    } yield wrappingCode

    //r.map(t => Stream.emits(t.sections))
    r.flatMap(t => {
      System.err.println(s"Number of instrumented blocks $blockCount. Number of instructions $instructionCount")
      Stream.emits(t.sortedSections)
    })
  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](wasi: Boolean): CoverageListener[F] = {

    new CoverageListener[F](wasi)

  }
}
