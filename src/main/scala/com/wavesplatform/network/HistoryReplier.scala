package com.wavesplatform.network

import com.wavesplatform.utils.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.network.message.BlockMessageSpec
import scorex.transaction.History
import scorex.utils.ScorexLogging

@Sharable
class HistoryReplier(history: History, maxChainLength: Int) extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case GetSignatures(otherSigs) =>

      log.info(s"Got GetSignaturesMessage with ${otherSigs.length} sigs within")

      otherSigs.foreach { parent =>
        val headers = history.blockIdsAfter(parent.bytes, maxChainLength).map(ByteStr(_))

        if (headers.nonEmpty) {
          ctx.write(Signatures(Seq(parent) ++ headers))
        }
      }

      ctx.flush()

    case GetBlock(sig) =>
      for (h <- history.heightOf(sig.bytes); bytes <- history.blockBytes(h)) {
        ctx.writeAndFlush(RawBytes(BlockMessageSpec.messageCode, bytes))
      }

    case _ => super.channelRead(ctx, msg)
  }
}
