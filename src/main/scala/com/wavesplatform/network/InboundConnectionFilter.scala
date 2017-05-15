package com.wavesplatform.network

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}

class InboundConnectionFilter extends ChannelInboundHandlerAdapter {
  private def allowConnection: Boolean = true

  override def channelActive(ctx: ChannelHandlerContext) =
    if (allowConnection) {
      ctx.pipeline().remove(this)
      super.channelActive(ctx)
    } else {
      ctx.close()
    }
}
