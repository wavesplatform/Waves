package com.wavesplatform.network

import com.wavesplatform.UtxPool
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging

@Sharable
class UtxPoolSynchronizer(utx: UtxPool, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case t: Transaction => utx.putIfNew(t, Some(ctx.channel())) match {
      case Left(TransactionValidationError(_, e)) =>
        log.debug(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Left(e) =>
        log.debug(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Right(_) =>
        log.debug(s"${id(ctx)} Added transaction ${t.id} to UTX pool")
    }
    case _ => super.channelRead(ctx, msg)
  }
}
