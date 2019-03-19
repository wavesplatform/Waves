package com.wavesplatform.network

import java.util

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings._
import com.wavesplatform.state.BlockchainUpdated
import io.netty.bootstrap.Bootstrap
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.util.concurrent.DefaultThreadFactory
import monix.execution.{Ack, Scheduler}
import monix.execution.Ack.Continue
import monix.reactive.{Observable, Observer}
import com.wavesplatform.protobuf.events.{PBBlockchainUpdated, PBEvents}
import io.netty.buffer.ByteBuf
import io.netty.handler.codec.{MessageToByteEncoder, MessageToMessageEncoder}
import io.netty.handler.codec.protobuf.ProtobufEncoder

private object PBInt32LengthPrepender extends MessageToByteEncoder[ByteBuf] {
  @throws[Exception]
  override protected def encode(ctx: ChannelHandlerContext, msg: ByteBuf, out: ByteBuf): Unit = {
    val bodyLen: Int = msg.readableBytes
    out.ensureWritable(4 + bodyLen)
    out.writeInt(bodyLen)
    out.writeBytes(msg)
  }
}

private object PBScalaToJava extends MessageToMessageEncoder[PBBlockchainUpdated] {
  override def encode(ctx: ChannelHandlerContext, msg: PBBlockchainUpdated, out: util.List[AnyRef]): Unit = {
    out.add(PBBlockchainUpdated.toJavaProto(msg))
  }
}

private class BlockchainUpdateHandler(blockchainUpdated: Observable[BlockchainUpdated], scheduler: Scheduler) extends ChannelInboundHandlerAdapter {
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

    blockchainUpdated.subscribe(obs)(scheduler)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    cause.printStackTrace()
    ctx.close
  }
}

class BlockchainUpdateServer(settings: WavesSettings, blockchainUpdates: Observable[BlockchainUpdated], scheduler: Scheduler) {
  private val group = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-updates-group", true))
//  private val workerGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-updates-worker-group", true))

  private val channel = new Bootstrap()
    .group(group)
    .channel(classOf[NioSocketChannel])
    .handler(
      new PipelineInitializer[SocketChannel](
        Seq(
          PBInt32LengthPrepender,
          new ProtobufEncoder,
          PBScalaToJava,
          new BlockchainUpdateHandler(blockchainUpdates, scheduler)
        )
      )
    )
    .connect(settings.networkSettings.blockchainUpdatesAddress)
    .channel()

  def shutdown(): Unit =
    try {
      channel.close().await()
    } finally {
      group.shutdownGracefully().await()
//      bossGroup.shutdownGracefully().await()
    }
}
