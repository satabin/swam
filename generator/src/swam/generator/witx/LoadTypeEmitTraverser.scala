package swam.generator.witx

import swam.witx.traverser.TypesTraverser
import swam.witx.unresolved.{
  AliasType,
  ArrayType,
  BaseWitxType,
  BasicType,
  EnumType,
  FlagsType,
  Handle,
  Pointer,
  StructType,
  UnionType
}

/**
  * @author Javier Cabrera-Arteaga on 2020-03-23
  */
class LoadTypeEmitTraverser(prev: String, types: Map[String, BaseWitxType], offset: String = "", mem: String = "")
    extends TypesTraverser[String](types) {

  def concatOffsets(offset: String, prev: String) = {
    if (prev.isEmpty) offset
    else s"$offset + $prev"
  }

  override val basicTypeTraverser = {
    case (_, t: BasicType) =>
      t match {
        case BasicType.u8     => s"$mem.readByte(${concatOffsets(offset, prev)}).unsafeRunSync()\n"
        case BasicType.u16    => s"$mem.readShort(${concatOffsets(offset, prev)}).unsafeRunSync\n"
        case BasicType.u32    => s"$mem.readInt(${concatOffsets(offset, prev)}).unsafeRunSync\n"
        case BasicType.u64    => s"$mem.readLong(${concatOffsets(offset, prev)}).unsafeRunSync\n"
        case BasicType.s64    => s"$mem.readLong(${concatOffsets(offset, prev)}).unsafeRunSync\n"
        case BasicType.string => s"$mem.readInt(${concatOffsets(offset, prev)}).unsafeRunSync\n"
      }
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

    case (_, t: StructType) => s"${t.tpeName}($mem, ${concatOffsets(offset, prev)})"

  }

  override val handleTypeTraverser = {
    case (_, t: Handle) => s"$mem.readInt(${concatOffsets(offset, prev)}).unsafeRunSync\n"
  }

  override val unionTypeTraverser = {
    case (_, t: UnionType) =>
      s"${t.tpeName}($mem, ${concatOffsets(offset, prev)})"
  }

  override val arrayTypeTraverser = {
    case (n, t: ArrayType) => s"new ArrayInstance[${t.tpeName}](, Len, ${t.size} ,(i) => (mem, i)).values)\n"
  }

  override val pointerTypeTraverser = {
    case (_, p: Pointer) =>
      s"new Pointer[${getVal(p.tpe)}](mem.readInt($offset).unsafeRunSync, (i) => ${new LoadTypeEmitTraverser("", types, "i", "mem")
        .traverse("", p.tpe)}, (i, r) => ${new WriteTypeEmitTraverser("r", "", types, "i", mem)
        .traverse("", p.tpe)})"
  }
}
