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
package runtime

import internals.interpreter._

import scala.language.higherKinds

/** Raised when module compilation is not possible. */
final class CompileException(msg: String, inner: Throwable = null) extends SwamException(msg, inner)

/** Raised when a problem occurs at module instantiation or when required exports are missing. */
final class LinkException(msg: String, inner: Throwable = null) extends SwamException(msg, inner)

/** Raised when something goes wrong during module execution. */
sealed class RuntimeException(msg: String, inner: Throwable = null) extends SwamException(msg, inner)

/** Raised when a trap is raised in a module. */
final class TrapException(frame: StackFrame, msg: String) extends RuntimeException(msg)

/** Raised when call stack overflows. */
final class StackOverflowException(frame: StackFrame) extends RuntimeException("call stack exhausted")

/** Raised when trying to type interface elements with invalid types. */
final class ConversionException(msg: String) extends RuntimeException(msg)
