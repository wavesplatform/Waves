package com.wavesplatform.network

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.{NewTransactionHandler, Transaction}
import scorex.utils.ScorexLogging

class UtxPoolSynchronizer(handler: NewTransactionHandler, network: Network)
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case t: Transaction =>
      handler.onNewTransaction(t) match {
        case Left(e) => log.debug(s"${id(ctx)} Error processing transaction: $e")
        case Right(_) => network.broadcast(t, Some(ctx.channel()))
      }
    case _ => super.channelRead(ctx, msg)
  }
}
