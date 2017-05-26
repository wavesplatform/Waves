package com.wavesplatform.network

import java.util.concurrent.ScheduledFuture

import com.wavesplatform.utils.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class ExtensionSignaturesLoader(syncTimeout: FiniteDuration)
  extends ChannelDuplexHandler with ScorexLogging {
  import ExtensionSignaturesLoader._

  private var currentTimeout = Option.empty[ScheduledFuture[Unit]]
  private var lastKnownSignatures = Seq.empty[ByteStr]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case s: Signatures =>
      val (known, unknown) = s.signatures.span(id => lastKnownSignatures.contains(id))
      currentTimeout.foreach(_.cancel(true))
      currentTimeout = None
      known.lastOption.foreach { lastKnown =>
        log.debug(s"${ctx.channel().id().asShortText()}: got extension with ${known.length}/${s.signatures.length} known signatures")
        ctx.fireChannelRead(ExtensionIds(lastKnown, unknown))
      }
    case _ => super.channelRead(ctx, msg)
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LoadExtensionSignatures(sigs) if currentTimeout.isEmpty =>
      lastKnownSignatures = sigs

      log.debug(s"${ctx.channel().id().asShortText()}: Loading extension")

      currentTimeout = Some(ctx.executor().schedule(syncTimeout) {
        if (currentTimeout.nonEmpty) {
          log.warn(s"Timeout expired while loading extension")
          // todo: blacklist peer
        }
      })

      ctx.writeAndFlush(GetSignatures(sigs), promise)

    case LoadExtensionSignatures(sigs) =>
      log.debug("Received request to load signatures while waiting for extension, ignoring for now")
      promise.setSuccess()

    case _ => super.write(ctx, msg, promise)
  }
}

object ExtensionSignaturesLoader {
  case class LoadExtensionSignatures(lastKnownSignatures: Seq[ByteStr])
}
