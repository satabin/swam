/*
 * Copyright 2018 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam
package test
package util

import better.files._

import scala.reflect.macros.blackbox.Context

object TestMacros {

  def listdir(c: Context)(name: c.Tree, fun: c.Tree): c.Tree = {
    import c.universe._

    implicit val lift = Liftable[File] { f =>
      val path = f.toString
      q"_root_.better.files.File($path)"
    }

    name match {
      case Literal(Constant(name: String)) =>
        val files = ("runtime" / "test" / "resources" / "spec-test").glob("*.wast").toList
        val calls = for {
          file <- files
          //if !(file.name.contains("float") ||file.name.contains("f32") || file.name.contains("f64"))
        } yield {
          q"${file.nameWithoutExtension} - $fun($file)"
        }
        q"Tests{..$calls}"
      case _ =>
        c.abort(c.enclosingPosition, "`listdir` must take a string literal as first argument")
    }
  }

}
