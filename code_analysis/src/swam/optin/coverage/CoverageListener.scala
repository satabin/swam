package swam
package code_analysis
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener, InstructionWrapper}
import java.io._

import binary.custom._
import fs2.Stream
import scodec._
import scodec.bits._
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
  Export,
  Expr,
  ExternalKind,
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
import swam.binary.custom.{FunctionNames, NameSectionHandler}
import swam.code_analysis.coverage.utils.TransformationContext

/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
class CoverageListener[F[_]](wasi: Boolean)(implicit F: MonadError[F, Throwable]) extends InstructionListener[F] {
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
  var id = 100

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
        id += 1
        Vector(BrIf(lbl), i32.Const(id), Call(ctx.cbFuncIndex))
      }
      case (x, i) => {
        instructionCount += 1
        if (i == 0) {
          blockCount += 1
          id += 1
          Vector(i32.Const(id), Call(ctx.cbFuncIndex), x)
        } else
          Vector(x)
      }

    }
  }

  def instrument(sections: Stream[F, Section]): Stream[F, Section] = {

    // TODO patch custom names section

    val r = for {
      firstPass <- sections
        .fold(TransformationContext(Seq(), None, None, None, None, None)) {
          case (ctx, c: Section.Types) =>
            ctx.copy(sections = ctx.sections,
                     types = Option(c),
                     imported = ctx.imported,
                     code = ctx.code,
                     exports = ctx.exports,
                     names = ctx.names)
          case (ctx, c: Section.Custom) => // Patch removing custom section
            {
              c match {
                case Section.Custom("name", payload) =>
                  ctx.copy(sections = ctx.sections,
                           types = ctx.types,
                           imported = ctx.imported,
                           code = ctx.code,
                           exports = ctx.exports,
                           names = Option(c))
                case _ =>
                  ctx.copy(sections = ctx.sections :+ c,
                           types = ctx.types,
                           imported = ctx.imported,
                           code = ctx.code,
                           exports = ctx.exports)
              }

            }
          case (ctx, c: Section.Imports) => {
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              code = ctx.code,
              //imported = Option(c)
              imported = Option(Section.Imports(c.imports.appended(Import.Function("swam", "swam_cb", ctx.tpeIndex)))),
              exports = ctx.exports,
              names = ctx.names
            )
          }
          case (ctx, c: Section.Code) => {
            ctx.copy(sections = ctx.sections,
                     types = ctx.types,
                     imported = ctx.imported,
                     code = Option(c),
                     exports = ctx.exports,
                     names = ctx.names)
          }
          case (ctx, c: Section.Exports) =>
            ctx.copy(sections = ctx.sections,
                     types = ctx.types,
                     imported = ctx.imported,
                     code = ctx.code,
                     exports = Option(c),
                     names = ctx.names)
          case (ctx, c: Section) =>
            ctx.copy(sections = ctx.sections :+ c,
                     types = ctx.types,
                     imported = ctx.imported,
                     code = ctx.code,
                     exports = ctx.exports,
                     names = ctx.names)
        }

      ctx = firstPass.copy(
        sections = firstPass.sections,
        types = firstPass.types,
        code = firstPass.code,
        imported =
          if (firstPass.imported.isEmpty)
            Option(Section.Imports(Vector(Import.Function("env", "swam_cb", firstPass.tpeIndex))))
          else
            firstPass.imported,
        exports = firstPass.exports,
        names = firstPass.names
      )
      ctxExports = ctx.copy(
        sections = ctx.sections,
        types = ctx.types,
        code = ctx.code,
        imported = ctx.imported,
        exports = Option(
          Section.Exports(
            ctx.exports.get.exports.map(x =>
              Export(x.fieldName,
                     x.kind,
                     if (x.kind == ExternalKind.Function && x.index >= ctx.cbFuncIndex) x.index + 1 else x.index))
          )),
        names = ctx.names match {
          case Some(m) => {
            val decoded =
              NameSectionHandler.codec.decodeValue(m.payload) match {
                case Attempt.Successful(names) =>
                  Names(names.subsections.map {
                    case FunctionNames(fnames) =>
                      FunctionNames(fnames.map {
                        case (k: Int, m: String) => (if (k >= ctx.cbFuncIndex) k + 1 else k, m)
                      })
                    case x => x
                  })
                case _ => Names(Vector()) // simply ignore malformed name section
              }

            NameSectionHandler.codec.encode(decoded) match {
              case Attempt.Successful(bv) => Option(Section.Custom("name", bv))
              case Attempt.Failure(err)   => ctx.names
            }
          }
          case None => ctx.names
        }
      )

      wrappingCode = ctxExports.copy(
        sections = ctxExports.sections,
        types = ctxExports.types,
        code = Option(
          Section.Code(
            firstPass.code.get.bodies
              .map(f => FuncBody(f.locals, instrumentVector(f.code, ctxExports)))
          )
        ),
        imported = ctxExports.imported,
        exports = ctxExports.exports,
        names = ctxExports.names
      )

    } yield wrappingCode

    //r.map(t => Stream.emits(t.sections))
    r.flatMap(t => {
      System.err.println(t.names)
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
