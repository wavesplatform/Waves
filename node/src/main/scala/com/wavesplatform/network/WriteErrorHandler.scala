package com.wavesplatform.network

import java.nio.channels.ClosedChannelException

import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.*

@Sharable
class WriteErrorHandler extends ChannelOutboundHandlerAdapter with ScorexLogging {
  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit =
    ctx.write(
      msg,
      promise.unvoid().addListener { (chf: ChannelFuture) =>
        if (!chf.isSuccess) {
          chf.cause() match {
            case _: ClosedChannelException =>
              log.trace(s"${id(ctx.channel())} Channel closed while writing (${msg.getClass.getCanonicalName})")
            case _: java.io.IOException =>
              log.trace(s"java.io.IOException on ${id(ctx.channel())} Write failed (${msg.getClass.getCanonicalName})")
            case other =>
              log.debug(s"${id(ctx.channel())} Write failed (${msg.getClass.getCanonicalName})", other)
          }
        }
      }
    )
}
