package com.wavesplatform.it.network.client

import java.util

import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled._
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.ByteToMessageCodec
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.message.Message._
import scorex.utils.ScorexLogging

class LegacyFrameCodec extends ByteToMessageCodec[RawBytes] with ScorexLogging {
  import LegacyFrameCodec._
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = {
    require(in.readInt() == Magic, "invalid magic number")

    val code = in.readByte()
    val length = in.readInt()
    val dataBytes = new Array[Byte](length)
    if (length > 0) {
      val declaredChecksum = in.readSlice(ChecksumLength)
      in.readBytes(dataBytes)
      val actualChecksum = wrappedBuffer(FastCryptographicHash.hash(dataBytes), 0, ChecksumLength)

      require(declaredChecksum.equals(actualChecksum), "invalid checksum")
      actualChecksum.release()

    }

    out.add(RawBytes(code, dataBytes))
  }

  override def encode(ctx: ChannelHandlerContext, msg: RawBytes, out: ByteBuf) = {
    out.writeInt(Magic)
    out.writeByte(msg.code)
    if (msg.data.length > 0) {
      out.writeInt(msg.data.length)
      out.writeBytes(FastCryptographicHash.hash(msg.data), 0, ChecksumLength)
      out.writeBytes(msg.data)
    } else {
      out.writeInt(0)
    }
  }
}

object LegacyFrameCodec {
  val Magic = 0x12345678
}
