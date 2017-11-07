package com.wavesplatform.network

import com.wavesplatform.UtxPool
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging


@Sharable
class UtxPoolSynchronizer(utx: UtxPool, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.singleThread(
    "utx-pool-synchronizer",
    reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case t: Transaction => Task(utx.putIfNew(t) match {
      case Right(true) =>                             // log.trace(s"${id(ctx)} Added transaction ${t.id} to UTX pool")
        allChannels.broadcast(RawBytes(TransactionMessageSpec.messageCode, t.bytes()), Some(ctx.channel()))
      case Left(TransactionValidationError(e, _)) =>  // log.trace(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Left(e) =>                                 // log.trace(s"${id(ctx)} Error processing transaction ${t.id}: $e")
      case Right(false) =>                            // log.trace(s"${id(ctx)} TX ${t.id} already known")
    }).runAsync
    case _ => super.channelRead(ctx, msg)
  }
}
