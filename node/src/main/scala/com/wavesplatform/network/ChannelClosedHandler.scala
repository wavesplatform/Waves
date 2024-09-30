package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.*
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

@Sharable
class ChannelClosedHandler private extends ChannelHandlerAdapter {
  private val closedChannelsSubject = ConcurrentSubject.publish[Channel](Scheduler.global)

  override def handlerAdded(ctx: ChannelHandlerContext): Unit = {
    ctx.channel().closeFuture().addListener((cf: ChannelFuture) => closedChannelsSubject.onNext(cf.channel()))
    super.handlerAdded(ctx)
  }

  def shutdown(): Unit = {
    closedChannelsSubject.onComplete()
  }
}

object ChannelClosedHandler {
  def apply(): (ChannelClosedHandler, Observable[Channel]) = {
    val h = new ChannelClosedHandler()
    (h, h.closedChannelsSubject)
  }
}
