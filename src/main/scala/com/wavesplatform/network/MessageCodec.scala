package com.wavesplatform.network

import java.util

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success}

@Sharable
class MessageCodec(peerDatabase: PeerDatabase) extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {

  import BasicMessagesRepo.specsByCodes

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]) = msg match {
    // Have no spec
    case r: RawBytes => out.add(r)
    case LocalScoreChanged(score) => out.add(RawBytes(ScoreMessageSpec.messageCode, ScoreMessageSpec.serializeData(score)))
    case BlockForged(b) => out.add(RawBytes(BlockMessageSpec.messageCode, b.bytes()))

    // With a spec
    case GetPeers => out.add(RawBytes(GetPeersSpec.messageCode, Array[Byte]()))
    case k: KnownPeers => out.add(RawBytes(PeersSpec.messageCode, PeersSpec.serializeData(k)))
    case gs: GetSignatures => out.add(RawBytes(GetSignaturesSpec.messageCode, GetSignaturesSpec.serializeData(gs)))
    case s: Signatures => out.add(RawBytes(SignaturesSpec.messageCode, SignaturesSpec.serializeData(s)))
    case g: GetBlock => out.add(RawBytes(GetBlockSpec.messageCode, GetBlockSpec.serializeData(g)))
    case m: MicroBlockInv => out.add(RawBytes(MicroBlockInvMessageSpec.messageCode, MicroBlockInvMessageSpec.serializeData(m)))
    case m: MicroBlockRequest => out.add(RawBytes(MicroBlockRequestMessageSpec.messageCode, MicroBlockRequestMessageSpec.serializeData(m)))
    case m: MicroBlockResponse => out.add(RawBytes(MicroBlockResponseMessageSpec.messageCode, MicroBlockResponseMessageSpec.serializeData(m)))
  }

  override def decode(ctx: ChannelHandlerContext, msg: RawBytes, out: util.List[AnyRef]): Unit = {
    specsByCodes(msg.code).deserializeData(msg.data) match {
      case Success(x) => out.add(x)
      case Failure(e) => block(ctx, e)
    }
  }

  protected def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
    peerDatabase.blacklistAndClose(ctx.channel(), s"Invalid message. ${e.getMessage}")
  }

}
