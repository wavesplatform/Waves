package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, ReplayingDecoder}
import io.netty.util.concurrent.ScheduledFuture
import scorex.network.message.{BasicMessagesRepo, MessageSpec}
import scorex.network.{Handshake, TransactionalMessagesRepo}
import scorex.utils.ScorexLogging

class HandshakeDecoder extends ReplayingDecoder[Void] with ScorexLogging {
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = {
    out.add(Handshake.decode(in))
  }
}

object HandshakeDecoder {
  val Name = "handshake-decoder"
}

case object HandshakeTimeoutExpired

class HandshakeTimeoutHandler extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var timeout: Option[ScheduledFuture[_]] = None

  override def channelActive(ctx: ChannelHandlerContext) = {
    log.debug(s"Scheduling handshake timeout for ${ctx.channel().hashCode()}")
    timeout = Some(ctx.channel().eventLoop().schedule((() => {
      log.debug(s"FIRE TIMEOUT ${ctx.channel().hashCode()}")
      ctx.fireChannelRead(HandshakeTimeoutExpired)
    }): Runnable, 10, TimeUnit.SECONDS))

    super.channelActive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case hs: Handshake =>
      timeout.foreach(_.cancel(true))
      super.channelRead(ctx, hs)
    case other =>
      super.channelRead(ctx, other)
  }
}

object HandshakeTimeoutHandler {
  val Name = "handshake-timeout-handler"
}

@Sharable
class ClientHandshakeHandler(handshake: Handshake, connections: ConcurrentHashMap[(InetAddress, Long), Channel])
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case HandshakeTimeoutExpired => ctx.channel().close()
    case incomingHandshake: Handshake =>
      log.debug(s"Received handshake $incomingHandshake")
      val channelRemoteAddress = ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress]
      val remoteAddress = incomingHandshake.declaredAddress.getOrElse(channelRemoteAddress).getAddress
      if (connections.putIfAbsent(remoteAddress -> incomingHandshake.nodeNonce, ctx.channel()) != null) {
        log.debug(s"Already connected to peer, disconnecting")
        ctx.close()
      } else {
        ctx.writeAndFlush(handshake.encode(ctx.alloc().buffer()))
        ctx.pipeline().remove(HandshakeDecoder.Name)
        ctx.pipeline().remove(HandshakeTimeoutHandler.Name)
        ctx.pipeline().remove(ClientHandshakeHandler.Name)

        ctx.pipeline().addLast(new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4))
        val specs: Map[Byte, MessageSpec[_]] = (BasicMessagesRepo.specs ++ TransactionalMessagesRepo.specs).map(s => s.messageCode -> s).toMap
        ctx.pipeline().addLast("message-decoder", new MessageDecoder(specs))
      }
    case other =>
      log.debug(s"Unexpected message $other while waiting for handshake")
  }
}

object ClientHandshakeHandler {
  val Name = "handshake-handler"
}
