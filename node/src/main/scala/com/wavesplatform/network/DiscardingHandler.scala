package com.wavesplatform.network

import com.wavesplatform.utils.{Schedulers, ScorexLogging}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext}
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable

@Sharable
class DiscardingHandler(blockchainReadiness: Observable[Boolean], isLightMode: Boolean) extends ChannelDuplexHandler with ScorexLogging {

  private implicit val scheduler: SchedulerService = Schedulers.fixedPool(2, "discarding-handler")
  private val lastReadiness                        = lastObserved(blockchainReadiness)

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case RawBytes(code @ (TransactionSpec.messageCode | PBTransactionSpec.messageCode), _) if !lastReadiness().contains(true) || isLightMode =>
      logDiscarding(ctx, code)
    case RawBytes(code @ (BlockSnapshotResponseSpec.messageCode | MicroBlockSnapshotResponseSpec.messageCode), _) if !isLightMode =>
      logDiscarding(ctx, code)

    case _ => super.channelRead(ctx, msg)
  }

  private def logDiscarding(ctx: ChannelHandlerContext, code: Byte): Unit =
    log.trace(s"${id(ctx)} Discarding incoming message $code")
}
