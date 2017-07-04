package com.wavesplatform.network

import java.util.concurrent.Executors

import akka.dispatch.ExecutionContexts
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.{NewTransactionHandler, Transaction}
import scorex.utils.ScorexLogging

import scala.concurrent.Future

@Sharable
class UtxPoolSynchronizer(handler: NewTransactionHandler, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val executor = ExecutionContexts.fromExecutor(Executors.newSingleThreadExecutor())

  private def handleTransaction(ctx: ChannelHandlerContext, t: Transaction, remoteSource: Option[Channel]) = Future {
    val result = handler.onNewTransaction(t)
    result match {
      case Left(e) =>
        log.debug(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Right(_) =>
        log.trace(s"${id(ctx)} Added transaction ${t.id} to UTX pool")
        allChannels.broadcast(RawBytes(TransactionalMessagesRepo.TransactionMessageSpec.messageCode, t.bytes), remoteSource)
    }
    result
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case t: Transaction => handleTransaction(ctx, t, Some(ctx.channel()))
    case OffChainTransaction(t, p) =>
      log.trace(s"Handling off-chain transaction ${t.id}")
      handleTransaction(ctx, t, None).foreach { result =>
        result.left.foreach(ve => log.debug(s"Error processing off-chain transaction ${t.id }: $ve"))
        p.success(result)
      }
    case _ => super.channelRead(ctx, msg)
  }
}
