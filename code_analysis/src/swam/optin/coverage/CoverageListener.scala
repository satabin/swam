package swam
package code_analysis
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener, InstructionWrapper}
import java.io._

import cats.implicits._
import cats._
import cats.effect._
import scodec.bits.BitVector
import swam.syntax.{Block, Br, Call, CallIndirect, Expr, FuncBody, If, Import, Inst, Loop, MemorySize, Section, i32}
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
    // val ran = scala.util.Random

    /*

    case (ctx, c: Section.Code) =>
          ctx.copy(
            sections = ctx.sections :+
              ,
            types = ctx.types,
            imported = ctx.imported
          )
     */

    def instrumentVector(instr: Vector[Inst], ctx: TransformationContext): Vector[Inst] = {
      instr.flatMap {
        case CallIndirect(funcidx) =>
          if (funcidx >= ctx.cbFuncIndex) {
            Vector(
              CallIndirect(funcidx + 1)
            ) // Increment the call index for collision between previous and the new injected cb function
          } else {
            //println(s"Call $funcidx")
            Vector(CallIndirect(funcidx))
          }
        case Call(funcidx) =>
          if (funcidx >= ctx.cbFuncIndex) {
            println(s"Call replacement $funcidx")

            Vector(
              Call(funcidx + 1)
            ) // Increment the call index for collision between previous and the new injected cb function
          } else {
            //println(s"Call $funcidx")
            Vector(Call(funcidx))
          }
        case Block(tpe, instr) => {
          Vector(Block(tpe, instrumentVector(instr, ctx)))
        }
        case Loop(tpe, instr) => {
          Vector(Loop(tpe, instrumentVector(instr, ctx)))
        }
        case If(tpe, thn, els) => {
          Vector(If(tpe, instrumentVector(thn, ctx), instrumentVector(els, ctx)))
        }
        case x =>
          Vector(x)
      }
    }

    val r = for {
      firstPass <- sections
        .fold(TransformationContext(Seq(), None, None, None)) {
          case (ctx, c: Section.Types) =>
            ctx.copy(sections = ctx.sections, types = Option(c), imported = ctx.imported, code = ctx.code)
          case (ctx, c: Section.Custom) => // Patch removing custom section
            {
              if (c.name == "name") {
                NameSectionHandler.codec.decodeValue(c.payload) match {
                  case Attempt.Successful(names) => println(names)
                  case _                         => None
                }
              }
              ctx.copy(sections = ctx.sections, types = ctx.types, imported = ctx.imported, code = ctx.code)
            }
          case (ctx, c: Section.Imports) => {
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              code = ctx.code,
              //imported = Option(c)
              imported = Option(Section.Imports(c.imports.appended(Import.Function("env", "swam_cb", ctx.tpeIndex))))
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
              .map { x: FuncBody =>
                {
                  FuncBody(x.locals, instrumentVector(x.code, ctx))
                }
              })
        ),
        imported = ctx.imported
      )

    } yield wrappingCode

    //r.map(t => Stream.emits(t.sections))
    r.flatMap(t => {
      Stream.emits(t.sortedSections)
    })
  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](wasi: Boolean): CoverageListener[F] = {

    new CoverageListener[F](wasi)

  }
}
