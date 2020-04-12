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

/**
  * @author Javier Cabrera-Arteaga on 2020-03-23
  */
class WriteTypeEmitTraverser(f: String,
                             prev: String,
                             types: Map[String, BaseWitxType],
                             offset: String = "",
                             mem: String = "")
    extends TypesTraverser[String](types) {

  def concatOffsets(offset: String, prev: String) = {
    if (prev.isEmpty) offset
    else s"$offset + $prev"
  }

  override val basicTypeTraverser = {
    case (_, t: BasicType) =>
      t.name match {
        case "u8"     => s"$mem.writeByte(${concatOffsets(offset, prev)}, (`${f}` & 0xff).toByte )\n"
        case "u16"    => s"$mem.writeShort(${concatOffsets(offset, prev)},`${f}`)\n"
        case "u32"    => s"$mem.writeInt(${concatOffsets(offset, prev)},`${f}`)\n"
        case "u64"    => s"$mem.writeLong(${concatOffsets(offset, prev)},`${f}`)\n"
        case "s64"    => s"$mem.writeLong(${concatOffsets(offset, prev)},`${f}`)\n"
        case "string" => s"$mem.writeInt(${concatOffsets(offset, prev)},`${f}`)\n"
      }
  }

  override val aliasTypeTraverser = {
    case (_, t: AliasType) => traverse("", types(t.tpe.tpeName))
  }

  override val enumTypeTraverser = {
    case (_, t: EnumType) => traverse("", t.tpe)
  }

  override val flagsTypeTraverser = {
    case (_, t: FlagsType) => traverse("", t.tpe)
  }

  override val structTypeTraverser = {

    case (_, t: StructType) => s"$f.write(${concatOffsets(offset, prev)}, mem)"

  }

  override val handleTypeTraverser = {
    case (_, t: Handle) => s"$mem.writeInt(${concatOffsets(offset, prev)}, `${f}`)\n"
  }

  override val unionTypeTraverser = {
    case (_, t: UnionType) => s"$f.write(${concatOffsets(offset, prev)}, mem)"

  }

  override val arrayTypeTraverser = {
    case (_, t: ArrayType) => s"$mem.writeInt(${concatOffsets(offset, prev)}, `${f}`)\n"
  }
  override val pointerTypeTraverser = {
    case (_, p: Pointer) => s"$mem.writeInt(${concatOffsets(offset, prev)},`${f}`.offset)\n"
  }
}
