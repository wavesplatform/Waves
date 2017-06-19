package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelFuture, ChannelHandlerContext, ChannelPromise}
import scorex.utils.ScorexLogging

@Sharable
class ErrorHandler(blacklist: InetAddress => Unit) extends ChannelDuplexHandler with ScorexLogging {
  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) =
    ctx.write(msg, promise.unvoid().addListener { (chf: ChannelFuture) =>
      if (chf.isDone && chf.cause() != null) {
        log.debug(s"${id(ctx.channel())} Write failed", chf.cause())
      }
    })

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = {
    log.debug(s"${id(ctx)} Exception caught", cause)
    blacklist(ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress].getAddress)
  }
}
