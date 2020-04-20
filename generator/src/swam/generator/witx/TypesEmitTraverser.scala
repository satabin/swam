package swam.generator.witx

import swam.witx.traverser.TypesTraverser
import swam.witx.unresolved.{
  AliasType,
  ArrayType,
  BaseWitxType,
  BasicType,
  EnumType,
  Field,
  FlagsType,
  Handle,
  Pointer,
  StructType,
  UnionType
}

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
  * @author Javier Cabrera-Arteaga on 2020-03-21
  */
class TypesEmitTraverser(types: Map[String, BaseWitxType]) extends TypesTraverser[String](types) {

  override val basicTypeTraverser = {
    case (_, t: BasicType) =>
      t match {
        case BasicType.u8     => s"type ${t.tpeName} = Byte\n"
        case BasicType.u16    => s"type ${t.tpeName} = Short\n"
        case BasicType.u32    => s"type ${t.tpeName} = Int\n"
        case BasicType.u64    => s"type ${t.tpeName} = Long\n"
        case BasicType.s64    => s"type ${t.tpeName} = Long\n"
        case BasicType.string => s"type ${t.tpeName} = String\n"
        case BasicType.ptr    => s"type ${t.tpeName} = Int\n"
      }
  }

  val predefinedEnumVal: Map[String, String] = HashMap[String, String](
    "fd_datasync" -> "0x0000000000000001",
    "fd_read" -> "0x0000000000000002",
    "fd_seek" -> "0x0000000000000004",
    "fd_fdstat_set_flags" -> "0x0000000000000008",
    "fd_sync" -> "0x0000000000000010",
    "fd_tell" -> "0x0000000000000020",
    "fd_write" -> "0x0000000000000040",
    "fd_advise" -> "0x0000000000000080",
    "fd_allocate" -> "0x0000000000000100",
    "path_create_directory" -> "0x0000000000000200",
    "path_create_file" -> "0x0000000000000400",
    "path_link_source" -> "0x0000000000000800",
    "path_link_target" -> "0x0000000000001000",
    "path_open" -> "0x0000000000002000",
    "fd_readdir" -> "0x0000000000004000",
    "path_readlink" -> "0x0000000000008000",
    "path_rename_source" -> "0x0000000000010000",
    "path_rename_target" -> "0x0000000000020000",
    "path_filestat_get" -> "0x0000000000040000",
    "path_filestat_set_size" -> "0x0000000000080000",
    "path_filestat_set_times" -> "0x0000000000100000",
    "fd_filestat_get" -> "0x0000000000200000",
    "fd_filestat_set_size" -> "0x0000000000400000",
    "fd_filestat_set_times" -> "0x0000000000800000",
    "path_symlink" -> "0x0000000001000000",
    "path_remove_directory" -> "0x0000000002000000",
    "path_unlink_file" -> "0x0000000004000000",
    "poll_fd_readwrite" -> "0x0000000008000000",
    "sock_shutdown" -> "0x0000000010000000"
  )

  override val aliasTypeTraverser = {
    case (_, t: AliasType) => s"type ${t.tpeName}= ${t.tpe.tpeName}\n"
  }

  override val enumTypeTraverser = {
    case (_, t: EnumType) =>
      s"object ${t.tpeName}Enum extends Enumeration { \n\t ${t.names
        .map(t => s"\nval `$t` = Value${if (predefinedEnumVal.contains(t)) s"(${predefinedEnumVal(t)})" else " "}\n")
        .mkString("\n")}}\n"
  }

  override val flagsTypeTraverser = {
    case (_, t: FlagsType) =>
      s"object ${t.tpeName}Flags extends Enumeration { ${t.names.zipWithIndex
        .map {
          case (name, i) =>
            s"\t\nval ${name} = Value${if (predefinedEnumVal.contains(name)) s"(${predefinedEnumVal(name)})"
            else s"($i)"}\n"
        }
        .mkString("\n")}}\n\n"
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

    case (_, t: StructType) => {

      s"""case class `${t.tpeName}`(mem: Memory[IO], offset: Int) extends WASI_STRUCT {
         |  ${t.fields.zipWithIndex
           .map {
             case (f, idx) =>
               s"val `${f.id}` = ${new LoadTypeEmitTraverser(t.fields.map(t => t.tpe.size).slice(0, idx).sum.toString, types, offset = "offset", mem = "mem")
                 .traverse("", f.tpe)}"
           }
           .mkString("\n")}
         
         def write(offset: Int, mem: Memory[IO]) = {
         | ${t.fields.zipWithIndex
           .map {
             case (f, idx) =>
               s"${new WriteTypeEmitTraverser(f.id, t.fields.map(t => t.tpe.size).slice(0, idx).sum.toString, types, "offset", "mem")
                 .traverse("", f.tpe)}"

           }
           .mkString("\n")}
         |}
         |}\n\n""".stripMargin
    }

  }

  override val unionTypeTraverser = {
    case (_, t: UnionType) => {

      s"""case class `${t.tpeName}`(mem: Memory[IO], offset: Int) extends WASI_STRUCT { // UNION
         |  ${t.fields.zipWithIndex
           .map {
             case (f, idx) =>
               s"val `${f.id}` = ${new LoadTypeEmitTraverser(t.fields.map(t => t.tpe.size).slice(0, idx).sum.toString, types, offset = "offset", mem = "mem")
                 .traverse("", f.tpe)}"
           }
           .mkString("\n")}
         
         def write(offset: Int, mem: Memory[IO]) = {
         | ${t.fields.zipWithIndex
           .map {
             case (f, idx) =>
               s"${new WriteTypeEmitTraverser(f.id, t.fields.map(t => t.tpe.size).slice(0, idx).sum.toString, types, "offset", "mem")
                 .traverse("", f.tpe)}"

           }
           .mkString("\n")}
         |}
         |}\n\n""".stripMargin
    }

  }

  override val handleTypeTraverser = {
    case (_, t: Handle) => s"type ${t.tpeName} = Int\n\n"
  }

  override val arrayTypeTraverser = {
    case (_, t: ArrayType) => s"type ${t.tpeName} = List[${getVal(t.tpe)}]\n"
  }

  override def traverseAll(zero: String, compose: (String, String) => String) =
    s"package swam\npackage wasi\n import cats.effect.IO \nimport swam.runtime.Memory \n\nobject Types { \n // This is an autogenerated file, do not change it  \n\n ${super
      .traverseAll(zero, compose)} }"

}
