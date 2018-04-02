package com.wavesplatform.discovery.network

import java.net.SocketAddress

import io.netty.channel.{ChannelDuplexHandler, ChannelFuture, ChannelHandlerContext, ChannelPromise}

import scala.concurrent.{Future, Promise}

class ExceptionHandler extends ChannelDuplexHandler {

  private val p               = Promise[Boolean]()
  val closed: Future[Boolean] = p.future

  override def close(ctx: ChannelHandlerContext, promise: ChannelPromise): Unit = {
    super.close(ctx, promise)
    if (!p.isCompleted)
      p.success(true)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    p.failure(cause)
  }

  override def connect(ctx: ChannelHandlerContext, remoteAddress: SocketAddress, localAddress: SocketAddress, promise: ChannelPromise): Unit = {
    ctx.connect(remoteAddress, localAddress, promise.addListener((future: ChannelFuture) => {
      if (!future.isSuccess) {
        p.failure(future.cause())
      }
    }))
  }

  override def write(ctx: ChannelHandlerContext, msg: Any, promise: ChannelPromise): Unit = {
    ctx.write(msg, promise.addListener((future: ChannelFuture) => {
      if (!future.isSuccess) {
        p.failure(future.cause())
      }
    }))
  }
}
