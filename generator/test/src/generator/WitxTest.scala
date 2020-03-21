package swam
package generator

import java.io
import java.nio.file.Paths

import utest.{TestSuite, Tests, test}

import runtime._
import swam.test.util._
import utest._
import better.files._
import cats.effect._
import fs2.io.file
import swam.text.parser
import swam.witx.WitxParser
import swam.witx.parser.{ImportContext, TypesParser}
import swam.witx.unresolved._
import swam.witx
import swam.witx.traverser.{ModuleInterfaceTraverser, TypesTraverser}

import scala.concurrent.ExecutionContext

class ModuleTraverse(module: ModuleInterface, types: Map[String, BaseWitxType])
    extends ModuleInterfaceTraverser[Unit](module) {

  case class Adapt(from: String, to: String = "")

  override val functionExportTraverser = {
    case (_, f: FunctionExport) => {
      println(s"def ${f.id}(${f.params.map(m => s"${m.id}:${mapTypeToWasm(m.tpe).from}").mkString(",")}) = {")

      processAdaptor(f.params)
      println()

      processAdaptor(f.results)
      println("}")
    }
  }

  def processAdaptor(fields: Seq[Field]) = {
    fields
      .map(p => (p.id, mapTypeToWasm(p.tpe)))
      .filter { case (_, adaptor) => adaptor.from != adaptor.to && !adaptor.to.isEmpty }
      .foreach {
        case (name, adaptor) => println(s"\tval ${name}Adapted: ${adaptor.to} = adapt[${adaptor.to}]($name)")
      }
  }

  def mapTypeToWasm(t: BaseWitxType): Adapt = t match {
    case x: BasicType  => mapBasicType(x)
    case x: AliasType  => mapAliasType(x)
    case x: EnumType   => Adapt("Int", s"${x.tpeName}Enum.Value")
    case x: FlagsType  => Adapt("Int", s"${x.tpeName}Flags.Value")
    case x: StructType => Adapt("Int", x.tpeName)
    case x: ArrayType  => Adapt("Int", x.tpeName)
    case x: UnionType  => Adapt("Int", x.tpeName)
    case x: Pointer    => Adapt("Int", s"Pointer[${getVal(x.tpe)}]")
    case x: Handle     => Adapt("Int", "Int")
  }

  def getVal(tpe: BaseWitxType): String = tpe match {
    case x: AliasType  => x.tpeName
    case x: BasicType  => x.tpeName
    case x: EnumType   => s"${x.tpeName}Enum.Value"
    case x: FlagsType  => s"${x.tpeName}Flags.Value"
    case x: Pointer    => s"Pointer[${getVal(x.tpe)}]"
    case x: Handle     => x.tpeName
    case x: StructType => x.tpeName
    case x: UnionType  => x.tpeName
  }

  def mapBasicType(t: BasicType): Adapt = t.name match {
    case "u8"     => Adapt("Int")
    case "u16"    => Adapt("Int", "Short")
    case "u32"    => Adapt("Int")
    case "u64"    => Adapt("Long")
    case "s64"    => Adapt("Long")
    case "string" => Adapt("Int", "String")
  }

  def mapAliasType(t: AliasType): Adapt = mapTypeToWasm(types(t.name))

}

class EmitTraverse(types: Map[String, BaseWitxType]) extends TypesTraverser[Unit](types) {

  override val basicTypeTraverser = {
    case (_, t: BasicType) =>
      t.name match {
        case "u8"     => println(s"type ${t.tpeName} = Byte")
        case "u16"    => println(s"type ${t.tpeName} = Short")
        case "u32"    => println(s"type ${t.tpeName} = Int")
        case "u64"    => println(s"type ${t.tpeName} = Long")
        case "s64"    => println(s"type ${t.tpeName} = Long")
        case "string" => println(s"type ${t.tpeName} = String")
      }
  }

  override val aliasTypeTraverser = {
    case (_, t: AliasType) =>
      println(s"type ${t.tpeName}= ${t.name}")
  }

  override val enumTypeTraverser = {
    case (_, t: EnumType) =>
      println(s"object ${t.tpeName}Enum extends Enumeration { ")
      print("\tval ")
      println(t.names.mkString("", ",", " = Value"))
      println("}")
  }

  override val flagsTypeTraverser = {
    case (_, t: FlagsType) =>
      println(s"object ${t.tpeName}Flags extends Enumeration { ")
      t.names.zipWithIndex.foreach {
        case (name, i) => println(s"\tval ${name} = Value($i)")
      }
      println("}")
  }

  def getVal(tpe: BaseWitxType): String = tpe match {
    case x: AliasType  => x.tpeName
    case x: BasicType  => x.tpeName
    case x: EnumType   => s"${x.tpeName}Enum.Value"
    case x: FlagsType  => s"${x.tpeName}Flags.Value"
    case x: Pointer    => s"Pointer[${getVal(x.tpe)}]"
    case x: Handle     => x.tpeName
    case x: StructType => x.tpeName
    case x: UnionType  => x.tpeName
  }

  override val structTypeTraverser = {
    case (_, t: StructType) =>
      print(s"case class `${t.tpeName}` (")

      print(
        t.fields
          .map(f => {
            s"\t`${f.id}`: ${getVal(f.tpe)}"
          })
          .mkString(","))

      println(")")
  }

  override val unionTypeTraverser = {
    case (_, t: UnionType) =>
      print(s"case class `${t.tpeName}`[${t.fields.indices.map(i => s"T$i").mkString(",")}] (")

      print(
        t.fields
          .map(f => {
            s"\t`${f.id}`: ${getVal(f.tpe)}"
          })
          .mkString(","))

      println(")")
  }

  override val handleTypeTraverser = {
    case (_, t: Handle) =>
      println(s"type ${t.tpeName} = Int")
  }

  override val arrayTypeTraverser = {
    case (_, t: ArrayType) =>
      println(s"type ${t.tpeName} = Array[${getVal(t.tpe)}]")
  }
}

object WitParser extends TestSuite {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  def runParse() = {
    val wasi_snaphot = Paths.get("generator/resources/wasi_witx/wasi_snapshot_preview1.witx")

    val parser = WitxParser[IO]
    val ctx = ImportContext[IO]()

    val (types, interface) = Blocker[IO]
      .use(blocker => {
        for {
          (types, instruction) <- parser.parseModuleInterface(wasi_snaphot, blocker, ctx)
        } yield (types, instruction)
      })
      .unsafeRunSync()

    new EmitTraverse(types).traverseAll({}, (_, _) => {})

    println()

    new ModuleTraverse(interface, types).traverseAll({}, (_, _) => {})

  }

  val tests = Tests {
    "parsing_witx" - runParse()
  }

}
