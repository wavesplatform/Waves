package com.wavesplatform.network

import java.util

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings._
import com.wavesplatform.state.BlockchainUpdated
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
import com.wavesplatform.protobuf.events.{PBBlockchainUpdated, PBEvents}
import io.netty.handler.codec.MessageToMessageEncoder
import io.netty.handler.codec.protobuf.{ProtobufEncoder, ProtobufVarint32LengthFieldPrepender}

private object PBScalaToJava extends MessageToMessageEncoder[PBBlockchainUpdated] {
  override def encode(ctx: ChannelHandlerContext, msg: PBBlockchainUpdated, out: util.List[AnyRef]): Unit = {
    out.add(PBBlockchainUpdated.toJavaProto(msg))
  }
}

private class BlockchainUpdateHandler(stateUpdates: Observable[BlockchainUpdated]) extends ChannelInboundHandlerAdapter {
  implicit def toByteString(bs: ByteStr): ByteString =
    ByteString.copyFrom(bs.arr)

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    val obs = new Observer.Sync[BlockchainUpdated] {
      override def onNext(evt: BlockchainUpdated): Ack = {
        ctx.writeAndFlush(PBEvents.protobuf(evt)).await().sync()
        Continue
      }

      override def onError(ex: Throwable): Unit = {
        ex.printStackTrace()
        ctx.close
      }

      override def onComplete(): Unit = ctx.close
    }

    // @todo proper scheduler
    stateUpdates.subscribe(obs)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    cause.printStackTrace()
    ctx.close
  }
}

class BlockchainUpdateServer(settings: WavesSettings, blockchainUpdates: Observable[BlockchainUpdated]) {
  private val bossGroup   = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-boss-group", true))
  private val workerGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-worker-group", true))

  private val serverChannel = new ServerBootstrap()
    .group(bossGroup, workerGroup)
    .channel(classOf[NioServerSocketChannel])
    .childHandler(
      new PipelineInitializer[SocketChannel](
        Seq(
          new ProtobufVarint32LengthFieldPrepender,
          new ProtobufEncoder,
          PBScalaToJava,
          new BlockchainUpdateHandler(blockchainUpdates)
        )
      )
    )
    .bind(settings.networkSettings.blockchainUpdatesAddress)
    .channel()

  def shutdown(): Unit =
    try {
      serverChannel.close().await()
    } finally {
      workerGroup.shutdownGracefully().await()
      bossGroup.shutdownGracefully().await()
    }
}
