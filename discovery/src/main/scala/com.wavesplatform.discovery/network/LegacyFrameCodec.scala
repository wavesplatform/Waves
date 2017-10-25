package com.wavesplatform.discovery.network

import java.util

import com.wavesplatform.network.{BasicMessagesRepo, RawBytes, id}
import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled.wrappedBuffer
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec._
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.message.Message.ChecksumLength
import scorex.network.message.MessageSpec
import scorex.utils.ScorexLogging

import scala.util.control.NonFatal

object LegacyFrameCodec {
  val Magic = 0x12345678

  private val messageSpecs: Map[Byte, MessageSpec[_ <: AnyRef]] =
    BasicMessagesRepo.specs.map(s => s.messageCode -> s).toMap
}

class LegacyFrameCodec() extends ByteToMessageCodec[RawBytes] with ScorexLogging {
  import LegacyFrameCodec._

  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = try {
    require(in.readInt() == Magic, "invalid magic number")

    val code = in.readByte()
    require(messageSpecs.contains(code), s"Unexpected message code $code")

    val spec = messageSpecs(code)
    val length = in.readInt()
    require(length <= spec.maxLength, s"${spec.messageName} length $length exceeds ${spec.maxLength}")

    val dataBytes = new Array[Byte](length)
    if (length > 0) {
      val declaredChecksum = in.readSlice(ChecksumLength)
      in.readBytes(dataBytes)
      val actualChecksum = wrappedBuffer(FastCryptographicHash.hash(dataBytes), 0, ChecksumLength)

      require(declaredChecksum.equals(actualChecksum), "invalid checksum")
      actualChecksum.release()
    }

    out.add(RawBytes(code, dataBytes))
  } catch {
    case NonFatal(e) =>
      log.warn(s"${id(ctx)} Malformed network message", e)
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