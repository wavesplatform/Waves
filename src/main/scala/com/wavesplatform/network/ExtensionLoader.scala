package com.wavesplatform.network

import java.util.concurrent.ScheduledFuture

import com.wavesplatform.settings.SynchronizationSettings
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.History
import scorex.utils.ScorexLogging

class ExtensionLoader(history: History, settings: SynchronizationSettings)
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var currentTimeout = Option.empty[ScheduledFuture[Unit]]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case ScoreObserver.NewHighScoreReceived if currentTimeout.isEmpty =>
      val lastBlockIds = history.lastBlockIds(settings.maxRollback)
      log.debug(s"Loading extension from ${ctx.channel().id().asShortText()} [$lastBlockIds]")

      ctx.writeAndFlush(GetSignatures(lastBlockIds))

      currentTimeout = Some(ctx.executor().schedule(settings.synchronizationTimeout) {
        if (currentTimeout.nonEmpty) {
          log.warn(s"Timeout expired while loading extension")
          // todo: blacklist peer
        }
      })

    case s: Signatures =>
      log.debug(s"Got extension with ${s.signatures.length} signatures")
      currentTimeout.foreach(_.cancel(false))
      currentTimeout = None
      ctx.fireChannelRead(s)

    case ScoreObserver.NewHighScoreReceived =>
      log.debug("Score changed while waiting for extension, ignoring for now")
    case _ => super.channelRead(ctx, msg)
  }
}
