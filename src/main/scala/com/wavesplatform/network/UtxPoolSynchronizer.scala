package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{NewTransactionHandler, Transaction}
import scorex.utils.ScorexLogging

@Sharable
class UtxPoolSynchronizer(handler: NewTransactionHandler, network: Network)
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case t: Transaction =>
      handler.onNewTransaction(t) match {
        case Left(TransactionValidationError(tx, err)) =>
          log.debug(s"${id(ctx)} Error processing transaction ${Base58.encode(tx.id)}: $err")
        case Left(e) =>
          log.debug(s"${id(ctx)} Error processing transaction ${Base58.encode(t.id)}: $e")
        case Right(_) =>
          log.debug(s"${id(ctx)} Added transaction ${Base58.encode(t.id)} to UTX pool")
          network.broadcast(t, Some(ctx.channel()))
      }
    case _ => super.channelRead(ctx, msg)
  }
}
