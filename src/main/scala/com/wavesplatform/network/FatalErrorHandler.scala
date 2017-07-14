package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.utils.ScorexLogging

import scala.util.control.NonFatal
import com.wavesplatform.utils.forceStopApplication


@Sharable
class FatalErrorHandler extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = cause match {
    case NonFatal(_) => log.debug(s"${id(ctx)} Exception caught", cause)
    case _ =>
      log.error(s"${id(ctx)} Fatal error in channel, terminating application", cause)
      forceStopApplication()
  }
}
