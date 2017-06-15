package com.wavesplatform.it.network.client

import java.util

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec
import scorex.network.message._
import scorex.utils.ScorexLogging

@Sharable
class MessageCodec(specs: Map[Byte, MessageSpec[_ <: AnyRef]])
    extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {
  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]) = msg match {
    case GetPeers => out.add(RawBytes(GetPeersSpec.messageCode, Array[Byte]()))
    case k: KnownPeers => out.add(RawBytes(PeersSpec.messageCode, PeersSpec.serializeData(k)))
    case gs: GetSignatures => out.add(RawBytes(GetSignaturesSpec.messageCode, GetSignaturesSpec.serializeData(gs)))
    case s: Signatures => out.add(RawBytes(SignaturesSpec.messageCode, SignaturesSpec.serializeData(s)))
    case g: GetBlock => out.add(RawBytes(GetBlockSpec.messageCode, GetBlockSpec.serializeData(g)))
    case r: RawBytes => out.add(r)
  }

  override def decode(ctx: ChannelHandlerContext, msg: RawBytes, out: util.List[AnyRef]) =
    out.add(specs(msg.code).deserializeData(msg.data).get)
}
