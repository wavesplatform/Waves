package com.wavesplatform.ride.runner.caches.disk

import com.google.common.primitives.{Ints, Longs, Shorts}

import java.io.OutputStream
import scala.util.chaining.scalaUtilChainingOps

trait OutputStreamSyntax {
  @inline implicit final def outputStreamOps(self: OutputStream): OutputStreamOps = new OutputStreamOps(self)
}

final class OutputStreamOps(private val self: OutputStream) extends AnyVal {
  def writeOpt(maybeXs: Option[Array[Byte]]): OutputStream = maybeXs match {
    case Some(xs) => writeBool(true).write(xs); self
    case None     => writeBool(false)
  }

  def writeWithIntLen(xs: Array[Byte]): OutputStream = writeInt(xs.length).tap(_.write(xs))
  def writeWithShortLen(xs: Array[Byte]): OutputStream = {
    require(xs.length.isValidShort, s"Length of array ${xs.length} should be a valid short")
    writeShort(xs.length.toShort).tap(_.write(xs))
  }

  def writeByte(x: Byte): OutputStream    = self.tap(_.write(x))
  def writeShort(x: Short): OutputStream  = self.tap(_.write(Shorts.toByteArray(x)))
  def writeInt(x: Int): OutputStream      = self.tap(_.write(Ints.toByteArray(x)))
  def writeLong(x: Long): OutputStream    = self.tap(_.write(Longs.toByteArray(x)))
  def writeBool(x: Boolean): OutputStream = writeByte(if (x) 1: Byte else 0: Byte)
}
