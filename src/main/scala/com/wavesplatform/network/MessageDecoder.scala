package com.wavesplatform.network

import java.util

import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled._
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.ByteToMessageDecoder
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.message.Message._
import scorex.network.message.MessageSpec

class MessageDecoder(specs: Map[MessageCode, MessageSpec[_]]) extends ByteToMessageDecoder {
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = {
    require(in.readInt() == 0x123456, "invalid handshake")
    val code = in.readByte()
    require(specs.contains(code), s"invalid message code $code")
    val length = in.readInt()
    val dataBytes = new Array[Byte](length)
    if (length > 0) {
      val declaredChecksum = in.readSlice(ChecksumLength)
      in.readBytes(dataBytes)
      val actualChecksum = wrappedBuffer(FastCryptographicHash.hash(dataBytes), 0, ChecksumLength)

      require(declaredChecksum.equals(actualChecksum), "invalid checksum")
      actualChecksum.release()

    }
    out.add(specs(code) -> dataBytes)
  }
}
