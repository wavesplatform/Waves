package com.wavesplatform.network

import com.wavesplatform.network.TransactionalMessagesRepo.TransactionMessageSpec
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext}
import scorex.utils.ScorexLogging

@Sharable
class DiscardingHandler(blockchainIsOutdated: => Boolean) extends ChannelDuplexHandler with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case RawBytes(code, _) if code == TransactionMessageSpec.messageCode && blockchainIsOutdated =>
      log.trace(s"${id(ctx)} Discarding incoming message $code")
    case _ => super.channelRead(ctx, msg)
  }
}
