package swam
package code_analysis
package coverage
package instrument

import cats.implicits._
import cats._
import cats.effect._
import fs2.Stream
import scodec.Attempt
import swam.binary.custom.{FunctionNames, NameSectionHandler, Names}
import swam.code_analysis.coverage.utils.{CoverageMetadaDTO, JSTransformationContext}
import swam.syntax.{
  Block,
  BrIf,
  Call,
  CallIndirect,
  Elem,
  Export,
  ExternalKind,
  FuncBody,
  If,
  Import,
  Inst,
  Loop,
  Section,
  i32
}

import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.writePretty

/**
  * @author Javier Cabrera-Arteaga on 2020-10-16
  */
class JSCallbackInstrumenter[F[_]](implicit F: MonadError[F, Throwable]) extends Instrumenter[F] {

  //TODO fix the count of instructions

  def instrumentVector(instr: Vector[Inst], ctx: JSTransformationContext): Vector[Inst] = {

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

  implicit val formats = DefaultFormats

  def instrument(sections: Stream[F, Section]): Stream[F, Section] = {

    val r = for {
      firstPass <- sections.zipWithIndex
        .fold(JSTransformationContext(Seq(), None, None, None, None, None, None, None)) {
          case (ctx, (c: Section.Types, i)) =>
            ctx.copy(
              types = Option((c, i))
            )
          case (ctx, (c: Section.Elements, i)) => {
            ctx.copy(
              elements = Option((c, i))
            )
          }
          case (ctx, (c: Section.Custom, i)) => // Patch removing custom section
            {
              c match {
                case Section.Custom("name", _) =>
                  ctx.copy(
                    names = Option((c, i))
                  )
                case _ =>
                  ctx.copy(
                    sections = ctx.sections.appended((c, i))
                  )
              }

            }
          case (ctx, (c: Section.Functions, i)) =>
            ctx.copy(
              functions = Option((c, i))
            )
          case (ctx, (c: Section.Imports, i)) => {
            ctx.copy(
              imported =
                Option((Section.Imports(c.imports.appended(Import.Function("swam", "swam_cb", ctx.tpeIndex))), i))
            )
          }
          case (ctx, (c: Section.Code, i)) => {
            ctx.copy(
              code = Option((c, i))
            )
          }
          case (ctx, (c: Section.Exports, i)) =>
            ctx.copy(
              exports = Option((c, i))
            )
          case (ctx, (c: Section, i)) =>
            ctx.copy(
              sections = ctx.sections.appended((c, i))
            )
        }

      ctx = firstPass.copy(
        imported =
          if (firstPass.imported.isEmpty)
            Option(
              (Section.Imports(Vector(Import.Function("swam", "swam_cb", firstPass.tpeIndex))),
               firstPass.imported.get._2))
          else
            firstPass.imported
      )
      ctxExports = ctx.copy(
        exports = Option(
          (Section.Exports(
             ctx.exports.get._1.exports.map(x =>
               Export(x.fieldName,
                      x.kind,
                      if (x.kind == ExternalKind.Function && x.index >= ctx.cbFuncIndex) x.index + 1 else x.index))
           ),
           ctx.exports.get._2)),
        names = ctx.names match { // The names section is not needed, only debugging reasons, TODO remove after
          case Some(m) => {
            val decoded =
              NameSectionHandler.codec.decodeValue(m._1.payload) match {
                case Attempt.Successful(names) =>
                  Option(Names(names.subsections.collect {
                    case FunctionNames(fnames) =>
                      FunctionNames(
                        fnames.toVector
                          .map {
                            case (k: Int, m: String) => (if (k >= ctx.cbFuncIndex) k + 1 else k, m)
                          }
                          .toMap
                          .updated(ctx.cbFuncIndex, "__swam_swam_cb"))
                  }))
                case _ => None // simply ignore malformed name section
              }
            decoded match {
              case Some(d) =>
                NameSectionHandler.codec.encode(d) match {
                  case Attempt.Successful(bv) => Option((Section.Custom("name", bv), m._2))
                  case Attempt.Failure(err)   => ctx.names
                }
              case None => ctx.names
            }

          }
          case None => ctx.names
        },
        functions = ctx.functions,
        elements = ctx.elements
      )

      wrappingCode = ctxExports.copy(
        code = Option(
          (Section.Code(
             ctxExports.code.get._1.bodies
               .map(f => FuncBody(f.locals, instrumentVector(f.code, ctxExports)))
           ),
           ctxExports.code.get._2)),
        elements = Option(
          (Section.Elements(
             ctx.elements.get._1.elements
               .map(t => Elem(t.table, t.offset, t.init.map(fi => if (fi >= ctx.cbFuncIndex) fi + 1 else fi)))),
           ctx.elements.get._2)
        )
      )

    } yield wrappingCode

    //r.map(t => Stream.emits(t.sections))
    r.flatMap(t => {

      // Output JSON with the metadata
      println(writePretty(new CoverageMetadaDTO(instructionCount, blockCount)))

      //System.err.println(s"Number of instrumented blocks $blockCount. Number of instructions $instructionCount")

      Stream.emits(t.sortedSections)
    })
  }
}
