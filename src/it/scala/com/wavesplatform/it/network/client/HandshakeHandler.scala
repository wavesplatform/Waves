package com.wavesplatform.it.network.client

import java.net.InetSocketAddress
import java.util
import java.util.concurrent.{ConcurrentMap, TimeUnit}

import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import io.netty.handler.codec.ReplayingDecoder
import io.netty.util.concurrent.ScheduledFuture
import scorex.utils.ScorexLogging

class HandshakeDecoder extends ReplayingDecoder[Void] with ScorexLogging {
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = {
    out.add(Handshake.decode(in))
  }
}

case object HandshakeTimeoutExpired

class HandshakeTimeoutHandler extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var timeout: Option[ScheduledFuture[_]] = None

  private def cancelTimeout(): Unit = timeout.foreach(_.cancel(true))

  override def channelActive(ctx: ChannelHandlerContext) = {
    log.trace(s"${id(ctx)} Scheduling handshake timeout")
    timeout = Some(ctx.channel().eventLoop().schedule((() => {
      ctx.fireChannelRead(HandshakeTimeoutExpired)
    }): Runnable, 10, TimeUnit.SECONDS))

    super.channelActive(ctx)
  }

  override def channelInactive(ctx: ChannelHandlerContext) = {
    cancelTimeout()
    super.channelInactive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case hs: Handshake =>
      cancelTimeout()
      super.channelRead(ctx, hs)
    case other =>
      super.channelRead(ctx, other)
  }
}

abstract class HandshakeHandler(
    localHandshake: Handshake,
    establishedConnections: ConcurrentMap[Channel, PeerInfo],
    connections: ConcurrentMap[PeerKey, Channel],
    blacklist: Channel => Unit) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private def removeHandshakeHandlers(ctx: ChannelHandlerContext, thisHandler: ChannelHandler): Unit = {
    ctx.pipeline().remove(classOf[HandshakeDecoder])
    ctx.pipeline().remove(classOf[HandshakeTimeoutHandler])
    ctx.pipeline().remove(thisHandler)
  }

  private def peerInfo(incomingHandshake: Handshake, channel: Channel): PeerInfo = PeerInfo(
    channel.remoteAddress().asInstanceOf[InetSocketAddress],
    incomingHandshake.declaredAddress,
    incomingHandshake.applicationName,
    incomingHandshake.applicationVersion,
    incomingHandshake.nodeName,
    incomingHandshake.nodeNonce)

  private def remoteAddress(ctx: ChannelHandlerContext, incomingHandshake: Handshake) =
    incomingHandshake.declaredAddress
      .getOrElse(ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress])

  def connectionNegotiated(ctx: ChannelHandlerContext): Unit

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case rhs: Handshake if localHandshake.applicationName != rhs.applicationName =>
      log.warn(s"Remote application name ${rhs.applicationName} does not match local ${localHandshake.applicationName}")
      blacklist(ctx.channel)
    case rhs: Handshake =>
      log.debug(s"${id(ctx)} Received handshake $rhs")
      val ra = remoteAddress(ctx, rhs)
      val key = PeerKey(ra.getAddress, rhs.nodeNonce)
      if (connections.putIfAbsent(key, ctx.channel()) != null) {
        log.debug(s"${id(ctx)} Already connected to peer, disconnecting")
        ctx.close()
      } else {
        removeHandshakeHandlers(ctx, this)
        ctx.channel().attr(AttributeKeys.NodeName).set(rhs.nodeName)
        ctx.channel().attr(AttributeKeys.RemoteAddress).set(ra)
        establishedConnections.put(ctx.channel(), peerInfo(rhs, ctx.channel()))
        connectionNegotiated(ctx)
        ctx.channel().closeFuture().addListener { f: ChannelFuture =>
          connections.remove(key, f.channel())
        }
        ctx.fireChannelRead(rhs)
      }
    case _ => super.channelRead(ctx, msg)
  }
}

object HandshakeHandler {
  @Sharable
  class Client(
      handshake: Handshake,
      establishedConnections: ConcurrentMap[Channel, PeerInfo],
      connections: ConcurrentMap[PeerKey, Channel],
      blacklist: Channel => Unit)
    extends HandshakeHandler(handshake, establishedConnections, connections, blacklist) {

    override def connectionNegotiated(ctx: ChannelHandlerContext) = {}

    override def channelActive(ctx: ChannelHandlerContext) = {
      ctx.writeAndFlush(handshake.encode(ctx.alloc().buffer()))
      super.channelActive(ctx)
    }
  }
}
