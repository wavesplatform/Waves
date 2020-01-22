package com.wavesplatform.network

import java.io.IOException

import com.wavesplatform.utils.{ScorexLogging, forceStopApplication}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}

import scala.util.control.NonFatal
@Sharable
class FatalErrorHandler extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = cause match {
    case ioe: IOException if ioe.getMessage == "Connection reset by peer" =>
      // https://stackoverflow.com/q/9829531
      // https://stackoverflow.com/q/1434451
      log.trace(s"${id(ctx)} Connection reset by peer")
    case NonFatal(_) =>
      log.debug(s"${id(ctx)} Exception caught", cause)
    case _ =>
      new Thread(() => {
        log.error(s"${id(ctx)} Fatal error in channel, terminating application", cause)
        forceStopApplication()
      }, "waves-platform-shutdown-thread").start()
  }
}
