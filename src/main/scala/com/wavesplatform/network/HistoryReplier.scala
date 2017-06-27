package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.network.message.BlockMessageSpec
import scorex.transaction.History
import scorex.utils.ScorexLogging

@Sharable
class HistoryReplier(history: History, maxChainLength: Int) extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case GetSignatures(otherSigs) =>
      otherSigs.view
        .map(parent => parent -> history.blockIdsAfter(parent, maxChainLength))
        .find(_._2.nonEmpty) match {
          case Some((parent, extension)) =>
            log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length}, found common parent $parent and sending ${extension.length} more signatures")
            ctx.writeAndFlush(Signatures(parent +: extension))
          case None if otherSigs.length == 1 && otherSigs.head == history.lastBlock.uniqueId =>
            // this is the special case when both nodes only have genesis block
            log.debug(s"${id(ctx)} Both local and remote nodes only have genesis block")
            ctx.writeAndFlush(Signatures(otherSigs))
          case _ =>
            log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length} signatures, but could not find an extension")
      }

    case GetBlock(sig) =>
      for (h <- history.heightOf(sig); bytes <- history.blockBytes(h)) {
        ctx.writeAndFlush(RawBytes(BlockMessageSpec.messageCode, bytes))
      }

    case _: Handshake =>
      ctx.writeAndFlush(LocalScoreChanged(history.score()))

    case _ => super.channelRead(ctx, msg)
  }
}
