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
      log.debug(s"${id(ctx)} Got GetSignaturesMessage with ${otherSigs.length} sigs within")

      otherSigs.foreach { parent =>
        val headers = history.blockIdsAfter(parent, maxChainLength)

        if (headers.nonEmpty) {
          ctx.write(Signatures(Seq(parent) ++ headers))
        }
      }

      ctx.flush()

    case GetBlock(sig) =>
      for (h <- history.heightOf(sig); bytes <- history.blockBytes(h)) {
        ctx.writeAndFlush(RawBytes(BlockMessageSpec.messageCode, bytes))
      }

    case _: Handshake =>
      ctx.writeAndFlush(LocalScoreChanged(history.score()))

    case _ => super.channelRead(ctx, msg)
  }
}
