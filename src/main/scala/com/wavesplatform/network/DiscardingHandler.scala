package com.wavesplatform.network

import com.wavesplatform.network.TransactionalMessagesRepo.TransactionMessageSpec
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.utils.ScorexLogging

@Sharable
class DiscardingHandler extends ChannelDuplexHandler with ScorexLogging {
  @volatile
  private var blockchainIsOutdated = true

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case RawBytes(code, _) if code == TransactionMessageSpec.messageCode && blockchainIsOutdated =>
      log.trace(s"${id(ctx)} Discarding incoming message $code")
    case _ => super.channelRead(ctx, msg)
  }

  override def write(ctx: ChannelHandlerContext, msg: scala.Any, promise: ChannelPromise) = msg match {
    case BlockchainExpired =>
      if (!blockchainIsOutdated) {
        log.debug("Blockchain expired")
      }
      blockchainIsOutdated = true
    case BlockchainUpdated =>
      if (blockchainIsOutdated) {
        log.debug("Blockchain updated")
      }
      blockchainIsOutdated = false
    case _ => super.write(ctx, msg, promise)
  }
}
