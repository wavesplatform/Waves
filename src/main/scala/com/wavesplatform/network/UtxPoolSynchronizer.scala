package com.wavesplatform.network

import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.{NewTransactionHandler, Transaction}
import scorex.utils.ScorexLogging

@Sharable
class UtxPoolSynchronizer(handler: NewTransactionHandler, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case t: Transaction =>
      handler.onNewTransaction(t) match {
        case Left(TransactionValidationError(tx, err)) =>
          log.debug(s"${id(ctx)} Error processing transaction ${tx.id}: $err")
        case Left(e) =>
          log.debug(s"${id(ctx)} Error processing transaction ${t.id}: $e")
        case Right(_) =>
          log.debug(s"${id(ctx)} Added transaction ${t.id} to UTX pool")
          allChannels.broadcast(t, Some(ctx.channel()))
      }
    case _ => super.channelRead(ctx, msg)
  }
}
