package swam.generator.witx

import swam.witx.traverser.{ModuleInterfaceTraverser, TypesTraverser}
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

  case class Adapt(from: String, to: String)

  override val functionExportTraverser = {
    case (_, f: FunctionExport) =>
      s"def ${f.id}Impl${f.params
        .map(f => s"${f.id}:${getVal(f.tpe)}")
        .mkString("(", ",", ")")}:${mapFieldsToTuple(f.results)}\n\n" +
        s"def ${f.id}(${processParameters(f.params).mkString(",")}) = {\n${processAdaptor(f.params)} \n\n ${processResults(f)} \n}\n\n"

  }

  def processParameters(fields: Seq[Field]): Seq[String] = {
    if (fields.isEmpty) Seq()
    else {
      val head = fields.head
      val adaptor = mapTypeToWasm(head.tpe)

      adaptor.to match {
        case "String" => Seq(s"${head.id}:${adaptor.from}", s"${head.id}Len:Int") ++ processParameters(fields.tail)
        case _        => Seq(s"${head.id}:${adaptor.from}") ++ processParameters(fields.tail)
      }
    }

    // ${f.params.map(m => s"${m.id}:${mapTypeToWasm(m.tpe).from}").mkString(",")}
  }

  def mapFieldsToTuple(fields: Seq[Field]) = fields.map(t => getVal(t.tpe)).mkString("(", ",", ")")

  def processAdaptor(fields: Seq[Field]) = {
    fields
      .map(p => (p, mapTypeToWasm(p.tpe)))
      .filter { case (_, adaptor) => adaptor.from != adaptor.to }
      .map {
        case (field, adaptor) =>
          s"\tval ${field.id}Adapted: ${adaptor.to} = ${new InitTypeEmitTraverser(field.id).traverse("", field.tpe)}"
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

    /*try {
      val st = checkRight(fd, 0)
      st.write(offset, mem)
      printMem()
      Types.errnoEnum.`success`
    } catch {
      case x: WASIException => x.errno
    }*/

    s"IO( try {\n ${if (to.nonEmpty)
      s"${f.id}Impl($args).id"
    else
      s"${f.id}Impl($args)"} } catch {\n case x: WASIException => x.errno.id } )"

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
    case "ptr"    => Adapt("Int", "Int")
  }

  def mapAliasType(t: AliasType): Adapt = mapTypeToWasm(types(t.tpe.tpeName))

  override def traverseAll(zero: String, compose: (String, String) => String) =
    s"package swam\npackage wasi\nimport Types._\nimport Header._ \nimport cats.effect._\nimport cats.effect.IO \nimport swam.runtime.Memory\nimport swam.runtime.imports.annotations.module\n@module\n  trait Module { var mem: Memory[IO] = null \n\n val name = \"wasi_snapshot_preview1\" \n\n ${super
      .traverseAll(zero, compose)}\n }"

  class InitTypeEmitTraverser(name: String) extends TypesTraverser[String](types) {

    override val basicTypeTraverser = {
      case (_, t: BasicType) =>
        t.name match {
          case "u8"     => name
          case "u16"    => name
          case "u32"    => name
          case "u64"    => name
          case "s64"    => name
          case "string" => s"getString(mem, $name, ${name}Len)"
          case "ptr"    => name
        }
    }

    override val aliasTypeTraverser = {
      case (_, t: AliasType) => traverse("", types(t.tpe.tpeName))
    }

    override val enumTypeTraverser = {
      case (_, t: EnumType) => s"${t.tpeName}Enum($name)"
    }

    override val flagsTypeTraverser = {
      case (_, t: FlagsType) => s"${t.tpeName}Flags($name)"
    }

    override val structTypeTraverser = {

      case (_, t: StructType) => s"${t.tpeName}(mem, $name)"

    }

    override val handleTypeTraverser = {
      case (_, t: Handle) => s"Handle($name)"
    }

    override val unionTypeTraverser = {
      case (_, t: UnionType) => s"${t.tpeName}(mem, $name)"

    }

    // new ArrayInstance[iovec](iovs, iovsLen, 8, (i) => `iovec`(mem, i)).values
    override val arrayTypeTraverser = {
      case (_, t: ArrayType) =>
        s"new ArrayInstance[${getVal(t.tpe)}]($name, ${name}Len, ${t.size} ,(i) => ${getVal(t.tpe)}(mem, i)).values"
    }

    override val pointerTypeTraverser = {
      case (_, p: Pointer) =>
        s"new Pointer[${getVal(p.tpe)}](mem.readInt($name).unsafeRunSync, (i) => ${new LoadTypeEmitTraverser("", types, mem = "mem", offset = "i")
          .traverse("", p.tpe)}, (i, r) => ${new WriteTypeEmitTraverser("r", "", types, mem = "mem", offset = "i")
          .traverse("", p.tpe)})"
    }
  }
}
