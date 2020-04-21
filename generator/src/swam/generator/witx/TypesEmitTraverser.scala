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

  override val aliasTypeTraverser = {
    case (_, t: AliasType) => s"type ${t.tpeName}= ${t.tpe.tpeName}\n"
  }

  override val enumTypeTraverser = {
    case (_, t: EnumType) =>
      s"object ${t.tpeName}Enum extends Enumeration { \n\t ${t.names
        .map(t => s"\nval `$t` = Value\n")
        .mkString("\n")}}\n"
  }

  override val flagsTypeTraverser = {
    case (_, t: FlagsType) =>
      s"object ${t.tpeName}Flags extends Enumeration { ${t.names.zipWithIndex
        .map {
          case (name, i) =>
            s"\t\nval ${name} = Value($i)\n"
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

      s"""case class `${t.tpeName}`(mem: Memory[IO], offset: Int) {
         |  ${t.fields.zipWithIndex
           .map {
             case (f, idx) =>
               s"val `${f.id}` = ${new LoadTypeEmitTraverser(t.fields.map(t => t.tpe.size + t.tpe.pad).slice(0, idx).sum.toString, types, offset = "offset", mem = "mem")
                 .traverse("", f.tpe)}"
           }
           .mkString("\n")}
         
         def write(offset: Int, mem: Memory[IO]) = {
         | ${t.fields.zipWithIndex
           .map {
             case (f, idx) =>
               s"${new WriteTypeEmitTraverser(f.id, t.fields.map(t => t.tpe.size + t.tpe.pad).slice(0, idx).sum.toString, types, "offset", "mem")
                 .traverse("", f.tpe)}"

           }
           .mkString("\n")}
         |}
         |}\n\n""".stripMargin
    }

  }

  override val unionTypeTraverser = {
    case (_, t: UnionType) => {

      s"""case class `${t.tpeName}`(mem: Memory[IO], offset: Int) { // UNION
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
}
