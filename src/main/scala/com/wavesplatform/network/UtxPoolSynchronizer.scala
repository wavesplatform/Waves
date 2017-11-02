package com.wavesplatform.network

import java.util.concurrent.Executors

import akka.dispatch.ExecutionContexts
import com.wavesplatform.UtxPool
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.DefaultThreadFactory
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging

import scala.concurrent.Future

@Sharable
class UtxPoolSynchronizer(utx: UtxPool, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val executor = ExecutionContexts.fromExecutor(
    Executors.newSingleThreadExecutor(new DefaultThreadFactory("utx-pool-synchronizer", true)))

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case t: Transaction => Future(utx.putIfNew(t) match {
      case Left(TransactionValidationError(e, _)) =>
    //      log.trace(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Left(e) =>
    //    log.trace(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Right(true) =>
        allChannels.broadcast(RawBytes(TransactionMessageSpec.messageCode, t.bytes), Some(ctx.channel()))
     //   log.trace(s"${id(ctx)} Added transaction ${t.id} to UTX pool")
      case Right(false) =>
     //   log.trace(s"${id(ctx)} TX ${t.id} already known")
    })
    case _ => super.channelRead(ctx, msg)
  }
}
