package com.wavesplatform.discovery.network

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}

class MessageHandler(handler: PartialFunction[(Any, ChannelHandlerContext), Unit]) extends ChannelInboundHandlerAdapter {
  override def channelRead(ctx: ChannelHandlerContext, msg: scala.Any): Unit = {
    handler((msg, ctx))
  }
}
