package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.utils.ScorexLogging

@Sharable
class WriteErrorHandler extends ChannelOutboundHandlerAdapter with ScorexLogging {
  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) =
    ctx.write(msg, promise.unvoid().addListener { (chf: ChannelFuture) =>
      if (!chf.isSuccess) {
        log.debug(s"${id(ctx.channel())} Write failed (${msg.getClass.getCanonicalName})", chf.cause())
      }
    })
}
