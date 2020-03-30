package swam
package impl
package JNA
package impl

import java.lang.reflect.{Field, Modifier}
import java.util

import com.sun.jna.Structure
import swam.impl.JNA.impl.annotations.PublicJNAField

import reflect.runtime.universe._
import scala.reflect.runtime.universe

/**
  * @author Javier Cabrera-Arteaga on 2020-03-30
  */
abstract class ScalaStructure extends Structure {

  private def getType[T](clazz: Class[T])(implicit runtimeMirror: Mirror) =
    runtimeMirror.classSymbol(clazz).toType

  private def getFieldList(cls: Class[_]): Array[Field] = {

    if (cls.equals(classOf[Structure]))
      Array()
    else {
      implicit val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader)
      val public =
        getType(cls).members
          .flatMap(
            f =>
              f.annotations
                .find(_.tree.tpe =:= typeOf[PublicJNAField])
                .map((cls.getDeclaredFields
                        .find(jf => jf.getName.equals(f.name.decodedName.toString.strip)),
                      _)))
          .collect {
            case (Some(x), _) => x
          }
          .toArray

      val default =
        cls.getDeclaredFields.filter(p => !Modifier.isStatic(p.getModifiers) && Modifier.isPublic(p.getModifiers))

      public.foreach(p => setFieldPublic(p, Modifier.PUBLIC)) // hack for scala fields to be public

      public ++ default ++ getFieldList(cls.getSuperclass)
    }
  }

  // TODO change to scala native access. This is really dangerous !!
  private def setFieldPublic(field: Field, newValue: Int = Modifier.PUBLIC): Unit = {
    field.setAccessible(true)
    val modifiersField = classOf[Field].getDeclaredField("modifiers")
    modifiersField.setAccessible(true)
    modifiersField.setInt(field, field.getModifiers | Modifier.PUBLIC)
  }

  override def getFieldList(): java.util.List[Field] = {
    val modifiersField = classOf[Field].getDeclaredField("modifiers")
    modifiersField.setAccessible(true)

    val result = new util.ArrayList[Field]()
    for (f <- getFieldList(getClass)) {
      result.add(f)
    }
    result
  }

  /*List flist = new ArrayList();
        for (Class cls = getClass();
             !cls.equals(Structure.class);
             cls = cls.getSuperclass()) {
            List classFields = new ArrayList();
            Field[] fields = cls.getDeclaredFields();
            for (int i=0;i < fields.length;i++) {
                int modifiers = fields[i].getModifiers();
                if (Modifier.isStatic(modifiers)
                    || !Modifier.isPublic(modifiers))
                    continue;
                classFields.add(fields[i]);
            }
            flist.addAll(0, classFields);
        }
        return flist;*/
}
