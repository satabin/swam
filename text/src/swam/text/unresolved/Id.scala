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
package text
package unresolved

sealed trait Id {
  def toOption: Option[String]
}
object Id {
  private val Valid = "^([0-9a-zA-Z!#$%&'*+-./:<=>?@\\^_`|~]+)$".r
  def fromOption(o: Option[String]): Id =
    o match {
      case Some(Valid(n)) => SomeId(n)
      case _              => NoId
    }
  def fromOptionOrElse(o: Option[String], dflt: Int): Either[Int, Id] =
    o match {
      case Some(Valid(n)) => Right(SomeId(n))
      case _              => Left(dflt)
    }
}
case class SomeId(id: String) extends Id {
  def toOption: Option[String] = Some(id)
}
case class FreshId(nb: Int) extends Id {
  def toOption: Option[String] = Some(s"#fresh-$nb")
}
case object NoId extends Id {
  def toOption: Option[String] = None
}
