package com.wavesplatform.network

import java.util

import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled._
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.ByteToMessageCodec
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.message.Message._
import scorex.network.message.{GetBlockSpec, GetSignaturesSpec, MessageSpec, SignaturesSpec}
import scorex.utils.ScorexLogging

class MessageCodec(specs: Map[MessageCode, MessageSpec[_ <: AnyRef]]) extends ByteToMessageCodec[Message] with ScorexLogging {
  import MessageCodec._
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = {
    require(in.readInt() == Magic, "invalid magic number")

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

    out.add(specs(code).deserializeData(dataBytes).get)
  }

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: ByteBuf) = {
    val (code, data) = msg match {
      case signatures: GetSignatures => (GetSignaturesSpec.messageCode, GetSignaturesSpec.serializeData(signatures))
      case signatures: Signatures => (SignaturesSpec.messageCode, SignaturesSpec.serializeData(signatures))
      case gb: GetBlock => (GetBlockSpec.messageCode, GetBlockSpec.serializeData(gb))
      case RawBytes(c, d) => (c, d)
    }

    writeData(code, data, out)
  }
}

object MessageCodec {
  val Magic = 0x12345678

  def writeData(code: Byte, data: Array[Byte], out: ByteBuf): Unit = {
    out.writeInt(Magic)
    out.writeByte(code)
    if (data.length > 0) {
      out.writeInt(data.length)
      out.writeBytes(FastCryptographicHash.hash(data), 0, ChecksumLength)
      out.writeBytes(data)
    } else {
      out.writeInt(0)
    }
  }
}
