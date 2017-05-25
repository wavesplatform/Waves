package com.wavesplatform.network

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.{NewTransactionHandler, Transaction}
import scorex.utils.ScorexLogging

class UtxPoolSynchronizer(handler: NewTransactionHandler, network: Network)
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case t: Transaction =>
      handler.onNewTransaction(t) match {
        case Left(e) => log.debug(s"Error processing transaction from ${ctx.channel().id().asShortText()}: $e")
        case Right(_) => network.broadcast(t, Some(ctx.channel()))
      }
    case _ => super.channelRead(ctx, msg)
  }
}
