package swam
package interpreter

import java.io.File
import java.nio.ByteBuffer

import cats.effect.IO
import swam.interpreter.InterpreterApp.{AsIIO, AsIsIO}
import swam.runtime.imports.{Imports, TCMap}
import swam.runtime.pageSize

import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._

import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader

/**
    @author Javier Cabrera-Arteaga on 2020-03-12
  */
class JVMLinker {

  // DEFAULT EMPTY IMPORTS
  private def buffer = {
    val buffer = ByteBuffer.allocate(2 * pageSize)
    buffer.limit(pageSize)
    buffer
  }

  def envPrintf(p0: Int, p1: Int): IO[Int] = {
    IO {
      println("Hello world")
      12
    }
  }

  private def imports() =
    Imports[IO](
      TCMap[String, AsIsIO](
        "env" -> TCMap[String, AsIIO](
          "memory" -> buffer,
          "printf" -> envPrintf _
        )
      ))
  //

  def getImports(jar: File, className: String) = {
    if (jar == null) { () =>
      imports()
    } else {
      System.err.println(s"Loading...${jar.getPath}")
      val child = new URLClassLoader(Seq(jar.toURI.toURL), this.getClass.getClassLoader)
      val classToLoad = Class.forName(className, true, child)
      val method = classToLoad.getDeclaredMethod("imports")
      val instance = classToLoad.newInstance
      () => method.invoke(instance).asInstanceOf[Imports[IO]]
    }
  }
}

object JVMLinker {
  def apply(): JVMLinker = new JVMLinker()
}
