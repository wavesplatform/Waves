package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.{NewTransactionHandler, Transaction}
import scorex.utils.ScorexLogging

@Sharable
class UtxPoolSynchronizer(handler: NewTransactionHandler, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private def handleTransaction(ctx: ChannelHandlerContext, t: Transaction, remoteSource: Option[Channel]) = {
    val result = handler.onNewTransaction(t)
    result match {
      case Left(e) =>
        log.debug(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Right(_) =>
        log.debug(s"${id(ctx)} Added transaction ${t.id} to UTX pool")
        allChannels.broadcast(t, remoteSource)
    }
    result
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case t: Transaction => handleTransaction(ctx, t, Some(ctx.channel()))
    case OffChainTransaction(t, p) => p.success(handleTransaction(ctx, t, None))
    case _ => super.channelRead(ctx, msg)
  }
}
