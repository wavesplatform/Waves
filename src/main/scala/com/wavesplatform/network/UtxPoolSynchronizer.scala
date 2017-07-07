package com.wavesplatform.network

import java.util.concurrent.Executors

import akka.dispatch.ExecutionContexts
import com.wavesplatform.UtxPool
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging

import scala.concurrent.Future

@Sharable
class UtxPoolSynchronizer(utx: UtxPool)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val executor = ExecutionContexts.fromExecutor(Executors.newSingleThreadExecutor())

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case t: Transaction => Future(utx.putIfNew(t, Some(ctx.channel())) match {
      case Left(TransactionValidationError(_, e)) =>
        log.debug(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Left(e) =>
        log.debug(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Right(_) =>
        log.trace(s"${id(ctx)} Added transaction ${t.id} to UTX pool")
    })
    case _ => super.channelRead(ctx, msg)
  }
}
