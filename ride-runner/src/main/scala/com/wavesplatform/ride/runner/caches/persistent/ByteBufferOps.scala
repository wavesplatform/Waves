package com.wavesplatform.ride.runner.caches.persistent

import cats.syntax.option.*
import com.google.common.primitives.{Ints, Longs, Shorts}

import java.nio.ByteBuffer

trait ByteBufferSyntax {
  @inline implicit final def outputStreamOps(self: ByteBuffer): ByteBufferOps = new ByteBufferOps(self)
}

final class ByteBufferOps(private val self: ByteBuffer) extends AnyVal {
  def readBytes(n: Int): Array[Byte] = {
    val r = new Array[Byte](n)
    self.get(r)
    r
  }

  def readOpt[T](x: => T): Option[T] = if (readBool()) x.some else none

  def readWithIntLen(): Array[Byte]   = readBytes(readInt())
  def readWithShortLen(): Array[Byte] = readBytes(readShort())

  def readByte(): Byte    = self.get()
  def readShort(): Short  = Shorts.fromBytes(readByte(), readByte())
  def readInt(): Int      = Ints.fromBytes(readByte(), readByte(), readByte(), readByte())
  def readLong(): Long    = Longs.fromBytes(readByte(), readByte(), readByte(), readByte(), readByte(), readByte(), readByte(), readByte())
  def readBool(): Boolean = readByte() == 1
}
