package swam
package impl
package JNA
package impl

import java.lang.reflect.{Field, Modifier}
import java.util

import com.sun.jna.Structure
import swam.impl.JNA.helper.ReflectionHelper
import swam.impl.JNA.impl.annotations.PublicJNAField

import reflect.runtime.universe._
import scala.reflect.runtime.universe

/**
  * @author Javier Cabrera-Arteaga on 2020-03-30
  */
abstract class ScalaStructure extends Structure {

  val fieldsMap: Array[Field] = getFieldList(getClass)

  override def getFieldOrder: util.List[_] =
    scala.collection.JavaConverters.asJava(getFieldList(getClass).map(t => t.getName))

  private def getFieldList(cls: Class[_], setPublic: Boolean = false): Array[Field] = {

    if (cls.equals(classOf[Structure]))
      Array()
    else {
      val public =
        ReflectionHelper()
          .getType(cls)
          .members
          .flatMap(
            f =>
              f.annotations
                .find(_.tree.tpe =:= typeOf[PublicJNAField])
                .map(t =>
                  (cls.getDeclaredFields
                     .find(jf => jf.getName.equals(f.name.decodedName.toString.strip)),
                   ReflectionHelper().getClassAnnotation[PublicJNAField](f))))
          .toList
          .collect {
            case (Some(x), Some(ann)) => (x, ann)
          }
          .sortBy(t => t._2.order)
          .collect { case (x, _) => x }
          .toArray

      val default =
        cls.getDeclaredFields.filter(p => !Modifier.isStatic(p.getModifiers) && Modifier.isPublic(p.getModifiers))

      if (setPublic)
        public.foreach(p => ReflectionHelper().setFieldPublic(p, Modifier.PUBLIC)) // hack for scala fields to be public

      public ++ default ++ getFieldList(cls.getSuperclass)
    }
  }

  override def getFieldList(): java.util.List[Field] =
    scala.collection.JavaConverters.asJava(getFieldList(getClass, setPublic = true))
}
