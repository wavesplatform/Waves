package com.wavesplatform.network

import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext}
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable

@Sharable
class DiscardingHandler(blockchainReadiness: Observable[Boolean]) extends ChannelDuplexHandler with ScorexLogging {

  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.fixedPool("discarding-handler", 2)
  private val lastReadiness                        = lastObserved(blockchainReadiness)

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case RawBytes(code, _) if code == TransactionSpec.messageCode && !lastReadiness().contains(true) =>
      log.trace(s"${id(ctx)} Discarding incoming message $code")
    case _ => super.channelRead(ctx, msg)
  }
}
