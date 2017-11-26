package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.utils.ScorexLogging

@Sharable
class TrafficLogger extends ChannelDuplexHandler with ScorexLogging {

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    log.trace(s"${id(ctx)} <-- $msg")
    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    log.trace(s"${id(ctx)} --> $msg")
    super.channelRead(ctx, msg)
  }

}
