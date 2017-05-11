package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util
import java.util.concurrent.TimeUnit

import com.google.common.base.Charsets
import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.handler.codec.ReplayingDecoder
import io.netty.util.concurrent.ScheduledFuture
import scorex.app.ApplicationVersion
import scorex.network.Handshake
import scorex.utils.ScorexLogging

@Sharable
object HandshakeDecoder extends ReplayingDecoder[Void] {
  val Name = "handshake-decoder"
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = {
    val appNameSize = in.readByte()
    val appName = in.toString(in.readerIndex(), appNameSize, Charsets.UTF_8)
    val appVersion = ApplicationVersion.parseByteBuf(in)
    val nodeNameSize = in.readByte()
    val nodeName = in.toString(in.readerIndex(), nodeNameSize, Charsets.UTF_8)
    val nonce = in.readLong()
    val fas = in.readInt()
    val isa = if (fas <= 0) None else {
      val address = InetAddress.getByAddress(in.readBytes(4).array())
      val port = in.readInt()
      Some(new InetSocketAddress(address, port))
    }
    val time = in.readLong()

    out.add(Handshake(appName, appVersion, nodeName, nonce, isa, time))
  }
}

case object HandshakeTimeoutExpired

class HandshakeTimeoutHandler extends ChannelInboundHandlerAdapter {
  private var timeout: Option[ScheduledFuture[_]] = None

  override def channelActive(ctx: ChannelHandlerContext) = {
    timeout = Some(ctx.channel().eventLoop().schedule((() => {
      ctx.fireChannelRead(HandshakeTimeoutExpired)
    }): Runnable, 5, TimeUnit.SECONDS))

    super.channelActive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case hs: Handshake =>
      timeout.foreach(_.cancel(true))
      super.channelRead(ctx, hs)
      ctx.channel().pipeline().remove(this)
    case other =>
      super.channelRead(ctx, other)
  }
}

object HandshakeTimeoutHandler {
  val Name = "handshake-timeout-handler"
}

@Sharable
class HandshakeHandler(hs: Handshake) extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelActive(ctx: ChannelHandlerContext) = {
    ctx.writeAndFlush(ctx.alloc().buffer().writeBytes(hs.bytes))
    super.channelActive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case HandshakeTimeoutExpired => ctx.channel().close()
    case hs: Handshake =>
      ctx.pipeline().remove(HandshakeDecoder.Name)
      ctx.pipeline().remove(HandshakeTimeoutHandler.Name)
      ctx.pipeline().remove(HandshakeHandler.Name)
      // todo: add actual codecs here
      ctx.pipeline().addLast("message-decoder", new MessageDecoder(???))
      super.channelRead(ctx, hs)
    case other =>
      log.debug(s"Unexpected message $other while waiting for handshake")
  }
}

object HandshakeHandler {
  val Name = "handshake-handler"
}
