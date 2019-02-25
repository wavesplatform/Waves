package com.wavesplatform.network

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings._
import com.wavesplatform.state.StateUpdateEvent
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.util.concurrent.DefaultThreadFactory
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.execution.Scheduler.Implicits.global
import monix.reactive.{Observable, Observer}

class StateUpdateHandler

class UpdateServerHandler(stateUpdates: Observable[StateUpdateEvent]) extends ChannelInboundHandlerAdapter {
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    val obs = new Observer.Sync[StateUpdateEvent] {
      override def onNext(elem: StateUpdateEvent): Ack = {
        // @todo serialize event
        val b: ByteStr = ByteStr.empty
        ctx.writeAndFlush(b).await().sync()
        Continue
      }

      override def onError(ex: Throwable): Unit = {
        ex.printStackTrace()
        ctx.close
      }

      override def onComplete(): Unit = ctx.clone
    }

    // @todo proper scheduler
    stateUpdates.subscribe(obs)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    cause.printStackTrace()
    ctx.close
  }
}

class StateUpdateServer(settings: WavesSettings, stateUpdates: Observable[StateUpdateEvent]) {
  private val bossGroup   = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-boss-group", true))
  private val workerGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-worker-group", true))

  private val serverChannel = new ServerBootstrap()
    .group(bossGroup, workerGroup)
    .channel(classOf[NioServerSocketChannel])
    .childHandler(
      new PipelineInitializer[SocketChannel](
        Seq(new UpdateServerHandler(stateUpdates))
      )
    )
    .bind(settings.networkSettings.stateUpdatesAddress)
    .channel()

  def shutdown(): Unit =
    try {
      serverChannel.close().await()
    } finally {
      workerGroup.shutdownGracefully().await()
      bossGroup.shutdownGracefully().await()
    }
}
