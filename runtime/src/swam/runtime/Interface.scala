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

import java.nio.ByteBuffer

import cats._

import scala.language.higherKinds

/** All elements that are part of the interface of an instance must implement
  * this interface.
  *
  * Each specialization defines a specific WebAssembly type that can be imported
  * or exported.
  */
sealed trait Interface[F[_], +T <: Type] {

  /** Returns the type of this interface element. */
  def tpe: T

}

/** Functions must implement this interface to be used by Swam.
  *
  */
abstract class Function[F[_]] extends Interface[F, FuncType] {

  /** Invokes the function and returns its result.
    *
    * Implementations must not throw any exception but
    * must encapsulate their effect and failure in an instance of `F`.
    */
  def invoke(parameters: Vector[Value], m: Option[Memory[F]]): F[Option[Value]]

}

/** Globals must implement this interface to be used by Swam.
  *
  */
abstract class Global[F[_]] extends Interface[F, GlobalType] {

  /** Returns the global value.
    */
  def get: Value

  /** Sets the global value.
    *
    * Implementations must return a failed `F` if `v` is
    * not of the correct type or not mutable.
    */
  def set(v: Value): F[Unit]

}

/** Tables must implement this interface to be used by Swam.
  * This interface is unsafe, meaning that no exception is expected when
  * calling any method on it.
  *
  * This interface is '''not''' thread-safe, concurrent modifications may
  * lead to undefined behaviors.
  *
  */
trait Table[F[_]] extends Interface[F, TableType] {

  /** The size of this table in term of elements. */
  def size: Int

  /** Returns the function at the given index in the table.
    * The element may be `null`, even though Swam should never
    * encounter such problem.
    *
    * Implementations must initialize empty cells to `null`.
    */
  def apply(idx: Int): Function[F]

  /** Sets the function at the given index in the table.
    *
    * This method is used by Swam to initialize the table upon module
    * instantiation and should not be used otherwise.
    */
  def update(idx: Int, f: Function[F]): Unit
}

/** Memories must implement this interface to be used by Swam.
  *
  * This interface is unsafe, meaning that no exception is expected when
  * calling any method on it.
  * The Swam engine ensures that this does not occur by running only validated
  * WebAssembly code, where size constraints are checked before execution.
  * Implementations must respect the contract of each method.
  *
  * This interface is '''not''' thread-safe, concurrent access to the same
  * instance may lead to undefined behaviors.
  *
  * @define boundaries Implementations need not check for boundaries in this
  *                    method since the Swam engine already checks it.
  */
trait Memory[F[_]] extends Interface[F, MemType] {

  /** Returns the size in bytes of this memory instance. */
  def size: Int

  /** Tries to grow the available space by `by` pages.
    *
    * Instances should use the constant [[swam.runtime.pageSize]] to ensure
    * the correct page size is always used even if it changes in subsequent versions.
    * This method should never throw an exception. If growing is not possible,
    * whatever the reason, it must simply return `false`.
    */
  def grow(by: Int): F[Boolean]

  /** Writes a byte at the given index in memory.
    *
    *  $boundaries
    */
  def writeByte(idx: Int, v: Byte): F[Unit]

  /** Reads a byte at the given index in memory.
    *
    *  $boundaries
    */
  def readByte(idx: Int): F[Byte]

  /** Writes a short at the given index in memory.
    *
    *  $boundaries
    */
  def writeShort(idx: Int, v: Short): F[Unit]

  /** Reads a short at the given index in memory.
    *
    *  $boundaries
    */
  def readShort(idx: Int): F[Short]

  /** Writes a integer at the given index in memory.
    *
    *  $boundaries
    */
  def writeInt(idx: Int, v: Int): F[Unit]

  /** Reads a integer at the given index in memory.
    *
    *  $boundaries
    */
  def readInt(idx: Int): F[Int]

  /** Writes a long at the given index in memory.
    *
    *  $boundaries
    */
  def writeLong(idx: Int, v: Long): F[Unit]

  /** Reads a long at the given index in memory.
    *
    *  $boundaries
    */
  def readLong(idx: Int): F[Long]

  /** Writes a float at the given index in memory.
    *
    *  $boundaries
    */
  def writeFloat(idx: Int, v: Float): F[Unit]

  /** Reads a float at the given index in memory.
    *
    *  $boundaries
    */
  def readFloat(idx: Int): F[Float]

  /** Writes a double at the given index in memory.
    *
    *  $boundaries
    */
  def writeDouble(idx: Int, v: Double): F[Unit]

  /** Reads a double at the given index in memory.
    *
    *  $boundaries
    */
  def readDouble(idx: Int): F[Double]

  /** Writes the bytes in the provided buffer at the given index in memory.
    *
    * This method is used by Swam to initialize the memory upon module
    * instantiation and should not be used otherwise.
    *
    *  $boundaries
    */
  def writeBytes(idx: Int, bytes: ByteBuffer): F[Unit]
}


import scala.concurrent._
import java.util.concurrent.locks._

import config._

trait Tracer{

    //  Events must be written in order
    val locker = new ReentrantLock
    val conf: EngineConfiguration
    val regex = conf.tracer.filter.r
  
    
    def innerTrace(eventName: String, args: Any*): () => Unit
  
    def traceEvent(eventName: String, args: Any*){
        // Filtering usong trace options
      executeOnBack(() => {
          eventName match {
            case regex(_*) => innerTrace(eventName, args:_*)()
          }
      })
    }
  
  
    private def executeOnBack(f: () => Unit) = {
       implicit val ec: ExecutionContext = ExecutionContext.global
       
      Future {
        blocking{
          try{
            locker.lock()
            f()
          }
          finally{
            locker.unlock()
          }
        }
      }
  
    }
  }