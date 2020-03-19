package swam.witx.parser

import swam.witx.unresolved.BaseWitxType

/**
  *@author Javier Cabrera-Arteaga on 2020-03-18
  */
class ImportContext(val context: Map[String, BaseWitxType]) {

  val types = context
  def load(path: String) = {}

  def getType(name: String): Unit = types(name)
}

object ImportContext {
  def apply(context: Map[String, BaseWitxType]): ImportContext = new ImportContext(context)

  def apply(): ImportContext = new ImportContext(Map[String, BaseWitxType]())
}
