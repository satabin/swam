package swam
package impl
package JNA
package helper

import java.lang.reflect.{Field, Modifier}

import reflect.runtime.universe._
import scala.reflect.runtime.universe
import scala.annotation.StaticAnnotation

/**
  * @author Javier Cabrera-Arteaga on 2020-03-26
  */
class ReflectionHelper {

  /*
   security issue in jdk https://stackoverflow.com/questions/56039341/get-declared-fields-of-java-lang-reflect-fields-in-jdk12
   */

  import java.lang.invoke.MethodHandles

  val lookup: MethodHandles.Lookup = MethodHandles.privateLookupIn(classOf[Field], MethodHandles.lookup)
  private val MODIFIERS = lookup.findVarHandle(classOf[Field], "modifiers", classOf[Int])

  val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader)

  def getProtectedField(klass: Class[_], fieldName: String): Field = {
    val field = klass.getDeclaredField(fieldName)
    field.setAccessible(true)
    field
  }

  // TODO change to scala native access. This is really dangerous !!
  def setFieldPublic(field: Field, newValue: Int = Modifier.PUBLIC): Unit = {
    field.setAccessible(true)
    println(classOf[Field].getDeclaredFields.length)
    MODIFIERS.set(field, field.getModifiers | Modifier.PUBLIC)
  }

  def getType[T](clazz: Class[T]) =
    mirror.classSymbol(clazz).toType

  def getProtectedFieldValue(klass: Class[_], fieldName: String, instance: Any): Any = {
    val f = getProtectedField(klass, fieldName)
    f.get(instance)
  }

  def getClassAnnotation[A: TypeTag](symbol: Symbol): Option[A] = {
    val typeAnnotation = typeOf[A]

    symbol.annotations
      .find(a => a.tree.tpe == typeAnnotation)
      .map { annotation =>
        /*
         *  Has error : scala.reflect.internal.Trees$Select cannot be cast to scala.reflect.api.Constants$ConstantApi
         *  when annotation field use default value
         *
         *  Has error : scala.reflect.internal.Trees$TypeApply cannot be cast to scala.reflect.api.Constants$ConstantApi
         *  when annotation field  type is collection
         */
        val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
        mirror
          .reflectClass(typeAnnotation.typeSymbol.asClass)
          .reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
          .asInstanceOf[A]
      }
  }
}

object ReflectionHelper {
  def apply(): ReflectionHelper = new ReflectionHelper()
}
