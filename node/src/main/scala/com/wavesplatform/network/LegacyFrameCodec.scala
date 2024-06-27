package com.wavesplatform.network

import com.google.common.cache.CacheBuilder

import java.util
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.crypto
import com.wavesplatform.network.BasicMessagesRepo.Spec
import com.wavesplatform.network.LegacyFrameCodec.{Magic, MessageRawData}
import com.wavesplatform.network.message.Message.*
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled.*
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.{ByteToMessageCodec, DecoderException}

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

abstract class LegacyFrameCodec(peerDatabase: PeerDatabase) extends ByteToMessageCodec[Any] with ScorexLogging {

  protected def filterBySpecOrChecksum(spec: BasicMessagesRepo.Spec, checkSum: Array[Byte]): Boolean
  protected def specsByCodes: Map[Byte, BasicMessagesRepo.Spec]
  protected def messageToRawData(msg: Any): MessageRawData
  protected def rawDataToMessage(rawData: MessageRawData): AnyRef

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = cause match {
    case e: DecoderException => peerDatabase.blacklistAndClose(ctx.channel(), s"Corrupted message frame: $e")
    case _                   => super.exceptionCaught(ctx, cause)
  }

  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]): Unit =
    try {
      require(in.readInt() == Magic, "invalid magic number")

      val code = in.readByte()
      require(specsByCodes.contains(code), s"Unexpected message code $code")

      val spec   = specsByCodes(code)
      val length = in.readInt()
      require(length <= spec.maxLength, s"${spec.messageName} message length $length exceeds ${spec.maxLength}")

      val dataBytes = new Array[Byte](length)
      val pushToPipeline = length == 0 || {
        val declaredChecksum = in.readSlice(ChecksumLength)
        in.readBytes(dataBytes)
        val rawChecksum    = crypto.fastHash(dataBytes)
        val actualChecksum = wrappedBuffer(rawChecksum, 0, ChecksumLength)

        require(declaredChecksum.equals(actualChecksum), "invalid checksum")
        actualChecksum.release()

        filterBySpecOrChecksum(spec, rawChecksum)
      }

      if (pushToPipeline) out.add(rawDataToMessage(MessageRawData(code, dataBytes)))
    } catch {
      case NonFatal(e) =>
        log.warn(s"${id(ctx)} Malformed network message", e)
        peerDatabase.blacklistAndClose(ctx.channel(), s"Malformed network message: $e")
        in.resetReaderIndex() // Cancels subsequent read tries, see Netty decode() documentation
    }

  override def encode(ctx: ChannelHandlerContext, msg1: Any, out: ByteBuf): Unit = {
    val msg = messageToRawData(msg1)

    out.writeInt(Magic)
    out.writeByte(msg.code)
    if (msg.data.length > 0) {
      out.writeInt(msg.data.length)
      out.writeBytes(crypto.fastHash(msg.data), 0, ChecksumLength)
      out.writeBytes(msg.data)
    } else {
      out.writeInt(0)
    }
  }
}

object LegacyFrameCodec {
  val Magic = 0x12345678
  case class MessageRawData(code: Byte, data: Array[Byte])
}

class LegacyFrameCodecL1(peerDatabase: PeerDatabase, receivedTxsCacheTimeout: FiniteDuration) extends LegacyFrameCodec(peerDatabase) {

  private val receivedTxsCache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(receivedTxsCacheTimeout.length, receivedTxsCacheTimeout.unit)
    .build[String, Object]()

  protected def specsByCodes: Map[MessageCode, Spec] = BasicMessagesRepo.specsByCodes

  protected def filterBySpecOrChecksum(spec: BasicMessagesRepo.Spec, checkSum: Array[Byte]): Boolean = {
    spec != TransactionSpec || {
      val actualChecksumStr = Base64.encode(checkSum)
      if (receivedTxsCache.getIfPresent(actualChecksumStr) == null) {
        receivedTxsCache.put(actualChecksumStr, LegacyFrameCodecL1.dummy)
        true
      } else false
    }
  }

  protected def messageToRawData(msg: Any): MessageRawData = {
    val rawBytes = (msg: @unchecked) match {
      case rb: RawBytes           => rb
      case tx: Transaction        => RawBytes.fromTransaction(tx)
      case block: Block           => RawBytes.fromBlock(block)
      case mb: MicroBlockResponse => RawBytes.fromMicroBlock(mb)
    }
    MessageRawData(rawBytes.code, rawBytes.data)
  }

  protected def rawDataToMessage(rawData: MessageRawData): AnyRef =
    RawBytes(rawData.code, rawData.data)
}

object LegacyFrameCodecL1 {
  private val dummy = new Object()
}
