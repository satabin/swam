package swam
package code_analysis
package coverage
package instrument

import cats._
import fs2.Stream
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.writePretty
import scodec.Attempt
import swam.binary.custom.{FunctionNames, NameSectionHandler, Names}
import swam.code_analysis.coverage.utils.{CoverageMetadaDTO, TransformationContext}
import swam.syntax._

/**
  * @author Javier Cabrera-Arteaga on 2020-10-16
  */
class InnerMemoryCallbackInstrumenter[F[_]](implicit F: MonadError[F, Throwable]) extends Instrumenter[F] {

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

  implicit val formats = DefaultFormats

  def instrument(sections: Stream[F, Section]): Stream[F, Section] = {

    val r = for {
      firstPass <- sections.zipWithIndex
        .fold(TransformationContext(Seq(), None, None, None, None, None, None, None)) {
          case (ctx, (c: Section.Types, i)) =>
            ctx.copy(
              sections = ctx.sections,
              types = Option((c, i)),
              imported = ctx.imported,
              code = ctx.code,
              exports = ctx.exports,
              names = ctx.names,
              functions = ctx.functions,
              elements = ctx.elements
            )
          case (ctx, (c: Section.Elements, i)) => {
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              imported = ctx.imported,
              code = ctx.code,
              exports = ctx.exports,
              names = ctx.names,
              functions = ctx.functions,
              elements = Option((c, i))
            )
          }
          case (ctx, (c: Section.Custom, i)) => // Patch removing custom section
            {
              c match {
                case Section.Custom("name", _) =>
                  ctx.copy(
                    sections = ctx.sections,
                    types = ctx.types,
                    imported = ctx.imported,
                    code = ctx.code,
                    exports = ctx.exports,
                    names = Option((c, i)),
                    functions = ctx.functions,
                    elements = ctx.elements
                  )
                case _ =>
                  ctx.copy(
                    sections = ctx.sections.appended((c, i)),
                    types = ctx.types,
                    imported = ctx.imported,
                    code = ctx.code,
                    exports = ctx.exports,
                    functions = ctx.functions,
                    elements = ctx.elements
                  )
              }

            }
          case (ctx, (c: Section.Functions, i)) =>
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              code = ctx.code,
              imported = ctx.imported,
              exports = ctx.exports,
              names = ctx.names,
              functions = Option((c, i)),
              elements = ctx.elements
            )
          case (ctx, (c: Section.Imports, i)) => {
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              code = ctx.code,
              imported =
                Option((Section.Imports(c.imports.appended(Import.Function("swam", "swam_cb", ctx.tpeIndex))), i)),
              exports = ctx.exports,
              names = ctx.names,
              functions = ctx.functions,
              elements = ctx.elements
            )
          }
          case (ctx, (c: Section.Code, i)) => {
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              imported = ctx.imported,
              code = Option((c, i)),
              exports = ctx.exports,
              names = ctx.names,
              functions = ctx.functions,
              elements = ctx.elements
            )
          }
          case (ctx, (c: Section.Exports, i)) =>
            ctx.copy(
              sections = ctx.sections,
              types = ctx.types,
              imported = ctx.imported,
              code = ctx.code,
              exports = Option((c, i)),
              names = ctx.names,
              functions = ctx.functions,
              elements = ctx.elements
            )
          case (ctx, (c: Section, i)) =>
            ctx.copy(
              sections = ctx.sections.appended((c, i)),
              types = ctx.types,
              imported = ctx.imported,
              code = ctx.code,
              exports = ctx.exports,
              names = ctx.names,
              functions = ctx.functions,
              elements = ctx.elements
            )
        }

      ctx = firstPass.copy(
        sections = firstPass.sections,
        types = firstPass.types,
        code = firstPass.code,
        imported =
          if (firstPass.imported.isEmpty)
            Option(
              (Section.Imports(Vector(Import.Function("swam", "swam_cb", firstPass.tpeIndex))),
               firstPass.imported.get._2))
          else
            firstPass.imported,
        exports = firstPass.exports,
        names = firstPass.names,
        functions = firstPass.functions,
        elements = firstPass.elements
      )
      ctxExports = ctx.copy(
        sections = ctx.sections,
        types = ctx.types,
        code = ctx.code,
        imported = ctx.imported,
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
        sections = ctxExports.sections,
        types = ctxExports.types,
        code = Option(
          (Section.Code(
             ctxExports.code.get._1.bodies
               .map(f => FuncBody(f.locals, instrumentVector(f.code, ctxExports)))
           ),
           ctxExports.code.get._2)),
        imported = ctxExports.imported,
        exports = ctxExports.exports,
        names = ctxExports.names,
        functions = ctxExports.functions,
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
