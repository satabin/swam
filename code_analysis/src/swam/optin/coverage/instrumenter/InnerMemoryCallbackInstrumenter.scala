package swam
package code_analysis
package coverage
package instrument

import cats._
import fs2.Stream
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.writePretty
import scodec.Attempt
import scodec.bits.BitVector
import swam.binary.custom.{FunctionNames, NameSectionHandler, Names}
import swam.code_analysis.coverage.utils.{CoverageMetadaDTO, InnerTransformationContext, JSTransformationContext}
import swam.syntax._
import swam.syntax.i32.Xor

/**
  * @author Javier Cabrera-Arteaga on 2020-10-16
  */
class InnerMemoryCallbackInstrumenter[F[_]](val coverageMemSize: Int = 1 << 10)(implicit F: MonadError[F, Throwable])
    extends Instrumenter[F] {

  val ran = scala.util.Random
  var blockCount = 0
  var instructionCount = 0
  var id = 1

  def instrumentVector(instr: Vector[Inst], ctx: InnerTransformationContext): Vector[Inst] = {

    instr.zipWithIndex.flatMap {
      case (CallIndirect(funcidx), i) => {
        instructionCount += 1
        Vector(CallIndirect(funcidx))
      }
      case (Call(funcidx), i) =>
        Vector(Call(funcidx))
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
        .fold(
          InnerTransformationContext(Seq(),
                                     None,
                                     None,
                                     None,
                                     None,
                                     None,
                                     None,
                                     None,
                                     None,
                                     None,
                                     0,
                                     -1,
                                     0,
                                     coverageMemSize)) {
          case (ctx, (c: Section.Types, i)) =>
            ctx.copy(
              types = Option((c, i))
            )
          case (ctx, (c: Section.Elements, i)) => {
            ctx.copy(
              elements = Option((c, i))
            )
          }
          case (ctx, (c: Section.Datas, i)) => {
            ctx.copy(data = Option((c, i)))
          }
          case (ctx, (c: Section.Custom, i)) => // Patch removing custom section
            {
              c match {
                case Section.Custom("name", _) =>
                  ctx.copy(
                    names = Option((c, Int.MaxValue))
                  )
                case _ =>
                  ctx.copy(
                    sections = ctx.sections.appended((c, i))
                  )
              }

            }
          case (ctx, (c: Section.Functions, i)) => {
            ctx.copy(
              functions = Option((c, i))
            )
          }
          case (ctx, (c: Section.Globals, i)) =>
            ctx.copy(
              globals = Option((c, i))
            )
          case (ctx, (c: Section.Imports, i)) => {
            ctx.copy(
              imported = Option((c, i))
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
        cbFuncIndex = firstPass.functions.get._1.functions.length + (firstPass.imported match {
          case Some(x) =>
            x._1.imports.collect {
              case x: Import.Function => x
            }.length
          case None => 0
        }), // Set new function index
        functions = Option(
          (
            Section.Functions(firstPass.functions.get._1.functions :+ firstPass.tpeIndex),
            firstPass.functions.get._2
          ))
      )

      ctxExports = ctx.copy(
        previousIdGlobalIndex = ctx.globals.get._1.globals.length,
        names = ctx.names match { // The names section is not needed, only debugging reasons, TODO remove after
          case Some(m) => {
            val decoded =
              NameSectionHandler.codec.decodeValue(m._1.payload) match {
                case Attempt.Successful(names) =>
                  Option(Names(names.subsections.collect {
                    case FunctionNames(fnames) =>
                      FunctionNames(fnames.updated(ctx.cbFuncIndex, "__swam_cb"))
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
        }
      )

      wrappingCode = ctxExports.copy(
        code = Option(
          (Section.Code(
             ctxExports.code.get._1.bodies
               .map(f => FuncBody(f.locals, instrumentVector(f.code, ctxExports)))
           ),
           ctxExports.code.get._2)),
        exports = Option(
          Section.Exports( // TODO patch with mnatch option in case the section does not exist
            ctxExports.exports.get._1.exports :+
              Export("__swam_cb", ExternalKind.Function, ctxExports.cbFuncIndex) // Callback function
              :+ Export("__previous_id", ExternalKind.Global, ctxExports.previousIdGlobalIndex) // Previous id index
              :+ Export("__afl_coverage_offset",
                        ExternalKind.Global,
                        ctxExports.previousIdGlobalIndex + 1) // AFL Coverage memory offset
              :+ Export("__afl_coverage_size",
                        ExternalKind.Global,
                        ctxExports.previousIdGlobalIndex + 2) // AFL Coverage memory size
              :+ Export("__block_coverage_offset",
                        ExternalKind.Global,
                        ctxExports.previousIdGlobalIndex + 3) // AFL Coverage memory size
              :+ Export("__block_coverage_size",
                        ExternalKind.Global,
                        ctxExports.previousIdGlobalIndex + 4) // AFL Coverage memory size
              :+ Export("__mem",
                        ExternalKind.Memory,
                        0) // Export memory, by default index 0 since only one memory is allowed
          ),
          ctxExports.exports.get._2
        ),
        blockCount = blockCount
      )

      dataCtx = wrappingCode.copy(
        globals = Option(
          Section.Globals(
            wrappingCode.globals.get._1.globals
              :+ Global(GlobalType(ValType.I32, Mut.Var), Vector(i32.Const(-1))) // Previous ID
              :+ Global(
                GlobalType(ValType.I32, Mut.Const),
                Vector(i32.Const(wrappingCode.lastDataOffsetAndLength._1.toInt))
              ) // MemoryOffset to collect AFL coverage memory
              :+ Global(GlobalType(ValType.I32, Mut.Const), Vector(i32.Const(coverageMemSize))) // AFLMemorySize
              :+ Global(
                GlobalType(ValType.I32, Mut.Const),
                Vector(i32.Const(wrappingCode.blockCoverageDataOffsetAndLength._1.toInt))
              ) // BLOCK Coverage offset
              :+ Global(GlobalType(ValType.I32, Mut.Const), Vector(i32.Const(blockCount + 1))) // BLOCK Coverage size
          ),
          wrappingCode.globals.get._2
        ),
        data = Option(
          Section.Datas(
            (wrappingCode.data match {
              case Some(realData) => realData._1.data
              case None           => Vector()
            })
              :+ // AFL Coverage memory piece
                Data(
                  0, // In the current version of WebAssembly, at most one memory is allowed in a module. Consequently, the only valid memidxmemidx is 00.
                  Vector(i32.Const(wrappingCode.lastDataOffsetAndLength._1.toInt)),
                  BitVector(new Array[Byte](coverageMemSize))
                )
              :+ // BLOCK Coverage memory piece
                Data(
                  0, // In the current version of WebAssembly, at most one memory is allowed in a module. Consequently, the only valid memidxmemidx is 00.
                  Vector(i32.Const(wrappingCode.blockCoverageDataOffsetAndLength._1.toInt)),
                  BitVector(new Array[Byte](wrappingCode.blockCoverageDataOffsetAndLength._2))
                )
          ),
          wrappingCode.data match {
            case Some(realData) => realData._2
            case None           => 11
          }
        ),
        code = Option(
          Section.Code(
            wrappingCode.code.get._1.bodies :+ FuncBody(
              Vector(),
              Vector(
                LocalGet(0), // l0
                i32.Const(1), // 1, l0
                i32.Store(0, wrappingCode.blockCoverageDataOffsetAndLength._1.toInt), // mem[offset + l0] = 1
                GlobalGet(wrappingCode.previousIdGlobalIndex),
                i32.Const(-1),
                i32.GtS,
                If(
                  BlockType.NoType,
                  Vector(
                    LocalGet(0), // l0
                    GlobalGet(wrappingCode.previousIdGlobalIndex), // g0, lo
                    Xor, // gx ^ l0
                    LocalGet(0), // l0, gx ^ l0
                    GlobalGet(wrappingCode.previousIdGlobalIndex),
                    Xor, // gx ^ l0, gx ^ l0,
                    i32.Load(0, wrappingCode.lastDataOffsetAndLength._1.toInt), // mem[gx ^ l0], gx ^ l0
                    i32.Const(1), // 1, mem[gx ^ l0], gx ^ l0
                    i32.Add, // 1 + mem[gx ^ l0], gx ^ l0
                    i32.Store(0, wrappingCode.lastDataOffsetAndLength._1.toInt), // mem[gx ^ l0] = 1 + mem[gx ^ l0]
                    LocalGet(0), // l0
                    i32.Const(1), // l0, 1
                    i32.ShrS, // l0 >> 1
                    GlobalSet(wrappingCode.previousIdGlobalIndex) // g0 = l0 >> 1
                  ),
                  Vector(
                    LocalGet(0),
                    GlobalSet(wrappingCode.previousIdGlobalIndex) // g0 = l0 >> 1
                  )
                )
              )
            )),
          wrappingCode.code.get._2
        )
      )

    } yield dataCtx

    //r.map(t => Stream.emits(t.sections))
    r.flatMap(t => {

      // Output JSON with the metadata
      println(writePretty(new CoverageMetadaDTO(instructionCount, blockCount, 1)))

      //System.err.println(s"Number of instrumented blocks $blockCount. Number of instructions $instructionCount")

      Stream.emits(t.sortedSections)
    })
  }
}
