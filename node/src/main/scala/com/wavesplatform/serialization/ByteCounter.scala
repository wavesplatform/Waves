package com.wavesplatform.serialization

import java.io.{DataInput, DataInputStream, InputStream}

import com.google.common.io.CountingInputStream
import io.netty.buffer.{ByteBuf, ByteBufInputStream}

trait ByteCounter {
  def position(): Long
}

object ByteCounter {
  def wrap(buf: ByteBuf): InputStream with DataInput with ByteCounter = new ByteBufInputStream(buf) with ByteCounter {
    override def position(): Long = buf.readerIndex()
  }

  def wrap(in: InputStream): InputStream with DataInput with ByteCounter = {
    val zzz = new CountingInputStream(in)
    new DataInputStream(zzz) with ByteCounter {
      override def position(): Long = zzz.getCount
    }
  }
}
