package com.wavesplatform.discovery.network

import java.util

import com.wavesplatform.network.{BasicMessagesRepo, BlockForged, BlockMessageSpec, GetBlock, GetBlockSpec, GetPeers, GetPeersSpec, GetSignatures, GetSignaturesSpec, KnownPeers, LocalScoreChanged, Message, MicroBlockInv, MicroBlockInvMessageSpec, MicroBlockRequest, MicroBlockRequestMessageSpec, MicroBlockResponse, MicroBlockResponseMessageSpec, PeersSpec, RawBytes, ScoreMessageSpec, Signatures, SignaturesSpec}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec
import scorex.network.message.MessageSpec
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success}

@Sharable
class MessageCodec() extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {

  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = BasicMessagesRepo.specs.map(s => s.messageCode -> s).toMap

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]): Unit = msg match {
    case GetPeers => out.add(RawBytes(GetPeersSpec.messageCode, Array[Byte]()))
    case r: RawBytes => out.add(r)
    case _ =>
  }

  override def decode(ctx: ChannelHandlerContext, msg: RawBytes, out: util.List[AnyRef]): Unit = {
    specs(msg.code).deserializeData(msg.data) match {
      case Success(x) => out.add(x)
      case Failure(e) => log.error(e.getMessage)
    }
  }
}
