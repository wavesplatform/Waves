package com.wavesplatform.network

import java.util.concurrent.ScheduledFuture

import com.wavesplatform.settings.SynchronizationSettings
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.transaction.History
import scorex.utils.ScorexLogging

class ExtensionSignaturesLoader(history: History, settings: SynchronizationSettings)
  extends ChannelDuplexHandler with ScorexLogging {
  import ExtensionSignaturesLoader._

  private var currentTimeout = Option.empty[ScheduledFuture[Unit]]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case s: Signatures =>
      val (known, unknown) = s.signatures.span(id => history.contains(id))
      currentTimeout.foreach(_.cancel(true))
      currentTimeout = None
      known.lastOption.foreach { lastKnown =>
        log.debug(s"${ctx.channel().id().asShortText()}: got extension with ${known.length}/${s.signatures.length} known signatures")
        ctx.fireChannelRead(ExtensionIds(lastKnown, unknown))
      }
    case _ => super.channelRead(ctx, msg)
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LoadSignatures if currentTimeout.isEmpty =>
      val lastBlockIds = history.lastBlockIds(settings.maxRollback)

      log.debug(s"Loading extension from ${ctx.channel().id().asShortText()}")

      currentTimeout = Some(ctx.executor().schedule(settings.synchronizationTimeout) {
        if (currentTimeout.nonEmpty) {
          log.warn(s"Timeout expired while loading extension")
          // todo: blacklist peer
        }
      })

      ctx.writeAndFlush(GetSignatures(lastBlockIds), promise)

    case LoadSignatures =>
      log.debug("Received request to load signatures while waiting for extension, ignoring for now")
      promise.setSuccess()

    case _ => super.write(ctx, msg, promise)
  }
}

object ExtensionSignaturesLoader {
  case object LoadSignatures
}
