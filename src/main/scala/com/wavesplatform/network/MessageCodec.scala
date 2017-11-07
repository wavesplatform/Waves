package com.wavesplatform.network

import java.util

import com.wavesplatform.metrics.Metrics
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec
import org.influxdb.dto.Point
import scorex.network.message._
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success}

@Sharable
class MessageCodec(peerDatabase: PeerDatabase) extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {

  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = BasicMessagesRepo.specs.map(s => s.messageCode -> s).toMap

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]) = msg match {
    case LocalScoreChanged(score) => out.add(RawBytes(ScoreMessageSpec.messageCode, ScoreMessageSpec.serializeData(score)))
    case GetPeers => out.add(RawBytes(GetPeersSpec.messageCode, Array[Byte]()))
    case k: KnownPeers => out.add(RawBytes(PeersSpec.messageCode, PeersSpec.serializeData(k)))
    case gs: GetSignatures => out.add(RawBytes(GetSignaturesSpec.messageCode, GetSignaturesSpec.serializeData(gs)))
    case s: Signatures => out.add(RawBytes(SignaturesSpec.messageCode, SignaturesSpec.serializeData(s)))
    case g: GetBlock => out.add(RawBytes(GetBlockSpec.messageCode, GetBlockSpec.serializeData(g)))
    case BlockForged(b) => out.add(RawBytes(BlockMessageSpec.messageCode, b.bytes()))
    case m: MicroBlockInv => out.add(RawBytes(MicroBlockInvMessageSpec.messageCode, MicroBlockInvMessageSpec.serializeData(m)))
    case m: MicroBlockRequest => out.add(RawBytes(MicroBlockRequestMessageSpec.messageCode, MicroBlockRequestMessageSpec.serializeData(m)))
    case m: MicroBlockResponse => out.add(RawBytes(MicroBlockResponseMessageSpec.messageCode, MicroBlockResponseMessageSpec.serializeData(m)))
    case r: RawBytes => out.add(r)
  }

  override def decode(ctx: ChannelHandlerContext, msg: RawBytes, out: util.List[AnyRef]): Unit = {
    val spec = specs(msg.code)
    spec.deserializeData(msg.data) match {
      case Success(x) =>
        Metrics.write(Point.measurement("message")
          .tag("type", spec.messageName)
          .addField("type", spec.messageName)
          .addField("bytes", msg.data.length))
        out.add(x)

      case Failure(e) => block(ctx, e)
    }
  }

  protected def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
    peerDatabase.blacklistAndClose(ctx.channel(), s"Invalid message. ${e.getMessage}")
  }

}
