package com.wavesplatform.network

import java.util.concurrent.{ConcurrentHashMap, ScheduledFuture}

import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.*

import scala.concurrent.duration.FiniteDuration

@Sharable
class BrokenConnectionDetector(timeout: FiniteDuration) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private val timeouts = new ConcurrentHashMap[Channel, ScheduledFuture[Unit]]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    scheduleClose(ctx)
    super.channelRead(ctx, msg)
  }

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    scheduleClose(ctx)
    super.channelActive(ctx)
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    Option(timeouts.remove(ctx.channel())).foreach(_.cancel(false))
    super.channelInactive(ctx)
  }

  private def scheduleClose(ctx: ChannelHandlerContext): Unit =
    Option(
      timeouts.put(
        ctx.channel(),
        ctx.executor().schedule(timeout) {
          log.info(s"${id(ctx.channel())} Channel haven't sent a message in $timeout, closing it")
          ctx.channel().close()
        }
      )
    ).foreach(_.cancel(false))
}
