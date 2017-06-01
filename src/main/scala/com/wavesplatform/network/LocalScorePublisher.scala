package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelOutboundHandlerAdapter, ChannelPromise}

@Sharable
class LocalScorePublisher(broadcast: AnyRef => Unit) extends ChannelOutboundHandlerAdapter {
  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = {
    msg match {
      case LocalScoreChanged(newLocalScore) =>
        broadcast(newLocalScore)
      case _ =>
    }
    super.write(ctx, msg, promise)
  }
}
