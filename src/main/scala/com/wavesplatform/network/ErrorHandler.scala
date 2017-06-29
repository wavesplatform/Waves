package com.wavesplatform.network

import java.net.InetSocketAddress

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.utils.ScorexLogging

import scala.util.control.NonFatal

@Sharable
class ErrorHandler(peerDatabase: PeerDatabase) extends ChannelOutboundHandlerAdapter with ScorexLogging {
  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) =
    ctx.write(msg, promise.unvoid().addListener { (chf: ChannelFuture) =>
      if (chf.isDone && chf.cause() != null) {
        log.debug(s"${id(ctx.channel())} Write failed (${msg.getClass.getCanonicalName})", chf.cause())
      }
    })
}

@Sharable
class FatalErrorHandler(peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = cause match {
    case NonFatal(_) =>
      log.debug(s"${id(ctx)} Exception caught", cause)
      peerDatabase.blacklist(ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress].getAddress)
    case _ =>
      log.error(s"Fatal error", cause)
      System.exit(1)
  }
}
