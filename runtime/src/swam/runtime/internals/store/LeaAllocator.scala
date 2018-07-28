/*
 * Copyright 2018 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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
package internals
package store

import cats._
import cats.implicits._

import java.nio.{ByteBuffer, ByteOrder}

import scala.annotation.tailrec

class LeaAllocator private (size: Int) {

  // port of Doug Lea’s allocator implemented at
  // https://github.com/odnoklassniki/one-nio/blob/master/src/one/nio/mem/Malloc.java

  import LeaAllocator._

  val capacity = size & ~7

  private val memory = ByteBuffer.allocate(capacity)
  memory.order(ByteOrder.LITTLE_ENDIAN)

  private var freeMemory = capacity

  def availableMemory: Int = freeMemory

  private def init(): Unit = {
    val start = BIN_SPACE
    val end = capacity - HEADER_SIZE * 2
    if (end - start < MIN_CHUNK) {
      throw new IllegalArgumentException("malloc area too small")
    } else {
      // Initialize the bins with the chunks of the maximum possible size
      @tailrec
      def loop(start: Int): Unit = {
        val size = math.min(end - start, MAX_CHUNK)
        addFreeChunk(start, size)
        addBoundary(start + size)
        freeMemory += size
        val start1 = start + size + HEADER_SIZE
        if (end - start1 >= MIN_CHUNK)
          loop(start1)
      }
      loop(start)
    }
  }

  /** Allocates the memory in the store for this data and returns the
    *  address into memory.
    *  If allocation fails, returns `-1`.
    */
  def allocate(size: Int): Int = {
    val alignedSize = (Math.max(size, 16) + (HEADER_SIZE + 7)) & ~7
    val bin = getBin(alignedSize)
    val adjustedSize = binSize(bin)
    val addr = mallocImpl(bin, adjustedSize)
    if (addr >= 0)
      addr
    else
      -1
  }

  private def mallocImpl(bin: Int, size: Int): Int = {
    @inline
    @tailrec
    def loop(bin: Int): Int = {
      val address = getChunk(bin, size)
      if (address >= 0)
        address + HEADER_SIZE
      else if (bin + 1 < BIN_COUNT)
        loop(bin + 1)
      else
        -1
    }
    loop(bin)
  }

  // Separate large chunks by occupied boundaries to prevent coalescing
  private def addBoundary(address: Int): Unit = {
    memory.putInt(address + SIZE_OFFSET, HEADER_SIZE | OCCUPIED_MASK)
    memory.putInt(address + HEADER_SIZE + LEFT_OFFSET, HEADER_SIZE)
  }

  // Find a suitable chunk starting from the given bin
  private def getChunk(bin: Int, size: Int): Int = {
    val binAddress = bin * BIN_SIZE
    val chunk = memory.getInt(binAddress + NEXT_OFFSET)
    if (chunk == 0) {
      -1
    } else {

      val chunkSize = memory.getInt(chunk + SIZE_OFFSET)
      val leftoverSize = chunkSize - size

      if (leftoverSize < MIN_CHUNK) {
        // Allocated memory perfectly fits the chunk
        memory.putInt(chunk + SIZE_OFFSET, chunkSize | OCCUPIED_MASK)
        freeMemory -= chunkSize
        removeFreeChunk(chunk)
        chunk
      } else {

        // Allocate memory from the best-sized chunk
        memory.putInt(chunk + SIZE_OFFSET, size | OCCUPIED_MASK)
        freeMemory -= size
        removeFreeChunk(chunk)

        // Cut off the remaining tail and return it to the bin as a smaller chunk
        val leftoverChunk = chunk + size
        addFreeChunk(leftoverChunk, leftoverSize)
        memory.putInt(leftoverChunk + LEFT_OFFSET, size)

        chunk
      }
    }
  }

  // Insert a new chunk in the head of the linked list of free chunks of a suitable bin
  private def addFreeChunk(address: Int, size: Int): Unit = {
    memory.putInt(address + SIZE_OFFSET, size)
    memory.putInt(address + size + LEFT_OFFSET, size)

    val binAddress = chooseBin(size) * BIN_SIZE
    val head = memory.getInt(binAddress + NEXT_OFFSET)
    memory.putInt(address + NEXT_OFFSET, head)
    memory.putInt(address + PREV_OFFSET, binAddress)
    memory.putInt(binAddress + NEXT_OFFSET, address)
    if (head != 0)
      memory.putInt(head + PREV_OFFSET, address)
  }

  // Remove a chunk from the linked list of free chunks
  private def removeFreeChunk(address: Int): Unit = {
    val next = memory.getInt(address + NEXT_OFFSET)
    val prev = memory.getInt(address + PREV_OFFSET)

    memory.putInt(prev + NEXT_OFFSET, next)
    if (next != 0)
      memory.putInt(next + PREV_OFFSET, prev)
  }

  def free(addr: Int): Unit = if (addr > 0) {
    var address = addr - HEADER_SIZE

    // Calculate the addresses of the neighbour chunks
    var size = memory.getInt(address + SIZE_OFFSET) & FREE_MASK
    val leftChunk = address - memory.getInt(address + LEFT_OFFSET)
    val rightChunk = address + size
    val leftSize = memory.getInt(leftChunk + SIZE_OFFSET)
    val rightSize = memory.getInt(rightChunk + SIZE_OFFSET)

    freeMemory += size

    // Coalesce with left neighbour chunk if it is free
    if (leftSize > 0) {
      size += leftSize
      removeFreeChunk(leftChunk)
      address = leftChunk
    }

    // Coalesce with right neighbour chunk if it is free
    if (rightSize > 0) {
      size += rightSize
      removeFreeChunk(rightChunk)
    }

    // Return the combined chunk to the bin
    addFreeChunk(address, size)
  }

  /** Returns a mutable sub-buffer for the allocated memory region
    *  at the given address.
    */
  def readMemory(addr: Int): ByteBuffer = {
    var address = addr - HEADER_SIZE
    var size = memory.getInt(address + SIZE_OFFSET) & FREE_MASK
    // TODO assumes no concurrent access, make it thread-safe somehow
    // but I really would like to avoid synchronization
    memory.position(addr)
    val slice = memory.slice()
    slice.limit(addr + size)
    slice
  }

  def read(addr: Int): Byte =
    memory.get(addr)

  def write(addr: Int, v: Byte): Unit =
    memory.put(addr, v)

  def writeBytes(addr: Int, v: Array[Byte]): Unit = {
    memory.position(addr)
    memory.put(v)
  }

  def readShort(addr: Int): Short =
    memory.getShort(addr)

  def writeShort(addr: Int, v: Short): Unit =
    memory.putShort(addr, v)

  def readInt(addr: Int): Int =
    memory.getInt(addr)

  def writeInt(addr: Int, v: Int): Unit =
    memory.putInt(addr, v)

  def readLong(addr: Int): Long =
    memory.getLong(addr)

  def writeLong(addr: Int, v: Long): Unit =
    memory.putLong(addr, v)

  def readFloat(addr: Int): Float =
    memory.getFloat(addr)

  def writeFloat(addr: Int, v: Float): Unit =
    memory.putFloat(addr, v)

  def readDouble(addr: Int): Double =
    memory.getDouble(addr)

  def writeDouble(addr: Int, v: Double): Unit =
    memory.putDouble(addr, v)

  def zero(address: Address, size: Int): Unit = {
    val zeroes = Array.fill[Byte](size)(0)
    memory.position(address)
    memory.put(zeroes)
  }

  def copy(from: Address, to: Address, size: Int): Unit = {
    memory.position(from)
    val fromSlice = memory.slice()
    fromSlice.limit(from + size)
    memory.position(to)
    val toSlice = memory.slice()
    toSlice.put(fromSlice)
  }

}

private object LeaAllocator {

  // Chunk header
  val HEADER_SIZE = 8
  val SIZE_OFFSET = 0
  val LEFT_OFFSET = 4
  val NEXT_OFFSET = 8
  val PREV_OFFSET = 16

  // Bins
  val BIN_COUNT = 120
  val BIN_SIZE = 8
  val BIN_SPACE = BIN_COUNT * BIN_SIZE + 64 // General header padded to 64 bytes

  // Chunk constraints
  val MAX_CHUNK = HEADER_SIZE + 1024 * 1024 * 1024
  val MIN_CHUNK = HEADER_SIZE + 16

  // Size flag that means the chunk is occupied
  val OCCUPIED_MASK = 0x80000000
  // Mask to extract the size of an occupied chunk
  val FREE_MASK = 0x7ffffff8

  // Calculate the address of the smallest bin which holds chunks of the given size.
  // Bins grow somewhat logarithmically: 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192 ...
  def getBin(size: Int): Int = {
    val size1 = size - (HEADER_SIZE + 1)
    val index = 29 - Integer.numberOfLeadingZeros(size1)
    (index << 2) + ((size1 >>> index) & 3)
  }

  // Get bin size including header
  def binSize(bin: Int): Int = {
    val bin1 = bin + 1
    ((4 + (bin1 & 3)) << (bin1 >>> 2)) + HEADER_SIZE
  }

  // Adjust bin to store values from bin size to next bin size excluding
  def chooseBin(size: Int): Int =
    getBin(size + 1) - 1

  def create(size: Int): LeaAllocator = {
    val allocator = new LeaAllocator(size)
    allocator.init()
    allocator
  }

}
