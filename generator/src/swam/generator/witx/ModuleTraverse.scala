package swam.generator.witx

import swam.witx.traverser.ModuleInterfaceTraverser
import swam.witx.unresolved.{
  AliasType,
  ArrayType,
  BaseWitxType,
  BasicType,
  EnumType,
  Field,
  FlagsType,
  FunctionExport,
  Handle,
  ModuleInterface,
  Pointer,
  StructType,
  UnionType
}

/**
  * @author Javier Cabrera-Arteaga on 2020-03-21
  */
class ModuleTraverse(module: ModuleInterface, types: Map[String, BaseWitxType])
    extends ModuleInterfaceTraverser[String](module) {

  case class Adapt(from: String, to: String = "")

  override val functionExportTraverser = {
    case (_, f: FunctionExport) =>
      s"def ${f.id}Impl${f.params
        .map(f => s"${f.id}:${getVal(f.tpe)}")
        .mkString("(", ",", ")")}:${mapFieldsToTuple(f.results)}\n\n" +
        s"def ${f.id}(${f.params.map(m => s"${m.id}:${mapTypeToWasm(m.tpe).from}").mkString(",")}) = {\n${processAdaptor(
          f.params)} \n\n ${processResults(f)} \n}\n\n"

  }

  def mapFieldsToTuple(fields: Seq[Field]) = fields.map(t => getVal(t.tpe)).mkString("(", ",", ")")

  def processAdaptor(fields: Seq[Field]) = {
    fields
      .map(p => (p.id, mapTypeToWasm(p.tpe)))
      .filter { case (_, adaptor) => adaptor.from != adaptor.to }
      .map {
        case (name, adaptor) => s"\tval ${name}Adapted: ${adaptor.to} = adapt[${adaptor.from}, ${adaptor.to}]($name)"
      }
      .mkString("\n")
  }

  def processResults(f: FunctionExport) = {

    val adaptors = f.results
      .map(p => mapTypeToWasm(p.tpe))

    val from = adaptors.map { x: Adapt =>
      x.from
    }
    val to = adaptors.map { x: Adapt =>
      x.to
    }

    val args = f.params
      .map(t => (t.id, mapTypeToWasm(t.tpe)))
      .map {
        case (name, adaptor) => if (adaptor.from == adaptor.to) s"$name" else s"${name}Adapted"
      }
      .mkString(",")

    println(s""""${f.id}" -> ${f.id} _,""")

    if (to.nonEmpty)
      s"IO(adapt[${to.mkString("(", ",", ")")}, ${from.mkString("(", ",", ")")}](${f.id}Impl($args)))"
    else
      s"IO(${f.id}Impl($args))"
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
    case x: ArrayType  => x.tpeName
  }

  def mapBasicType(t: BasicType): Adapt = t.name match {
    case "u8"     => Adapt("Int", "Int")
    case "u16"    => Adapt("Int", "Short")
    case "u32"    => Adapt("Int", "Int")
    case "u64"    => Adapt("Long", "Long")
    case "s64"    => Adapt("Long", "Long")
    case "string" => Adapt("Int", "String")
  }

  def mapAliasType(t: AliasType): Adapt = mapTypeToWasm(types(t.name))

  override def traverseAll(zero: String, compose: (String, String) => String) =
    s"package swam\npackage wasi\nimport Types._ \nimport cats.effect._\n  trait Module { def adapt[Tin, Tout](in: Tin): Tout \n\n${super
      .traverseAll(zero, compose)}\n }"
}
