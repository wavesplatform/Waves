package com.wavesplatform.network

import java.net.InetSocketAddress
import java.util
import java.util.concurrent.{ConcurrentMap, TimeUnit}

import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.handler.codec.ReplayingDecoder
import io.netty.util.concurrent.ScheduledFuture
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class HandshakeDecoder extends ReplayingDecoder[Void] with ScorexLogging {
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) =
    out.add(Handshake.decode(in))
}

case object HandshakeTimeoutExpired

class HandshakeTimeoutHandler(handshakeTimeout: FiniteDuration) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var timeout: Option[ScheduledFuture[_]] = None

  private def cancelTimeout(): Unit = timeout.foreach(_.cancel(true))

  override def channelActive(ctx: ChannelHandlerContext) = {
    log.trace(s"${id(ctx)} Scheduling handshake timeout")
    timeout = Some(ctx.channel().eventLoop().schedule((() => {
      ctx.fireChannelRead(HandshakeTimeoutExpired)
    }): Runnable, handshakeTimeout.toMillis, TimeUnit.MILLISECONDS))

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
    peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  import HandshakeHandler._

  def connectionNegotiated(ctx: ChannelHandlerContext): Unit

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case HandshakeTimeoutExpired =>
      peerDatabase.blacklistAndClose(ctx.channel(),"Timeout expired while waiting for handshake")
    case remoteHandshake: Handshake =>
      if (localHandshake.applicationName != remoteHandshake.applicationName)
        peerDatabase.blacklistAndClose(ctx.channel(),s"Remote application name ${remoteHandshake.applicationName} does not match local ${localHandshake.applicationName}")
       else if (!versionIsSupported(remoteHandshake.applicationVersion))
        peerDatabase.blacklistAndClose(ctx.channel(),s"Remote application version ${remoteHandshake.applicationVersion } is not supported")
       else {
        val key = PeerKey(ctx.remoteAddress.getAddress, remoteHandshake.nodeNonce)
        val previousPeer = connections.putIfAbsent(key, ctx.channel())
        if (previousPeer != null) {
          log.debug(s"${id(ctx)} Already connected to peer ${ctx.remoteAddress.getAddress} with nonce ${remoteHandshake.nodeNonce} on channel ${id(previousPeer)}")
          ctx.close()
        } else {
          log.debug(s"${id(ctx)} Accepted handshake $remoteHandshake")
          removeHandshakeHandlers(ctx, this)
          establishedConnections.put(ctx.channel(), peerInfo(remoteHandshake, ctx.channel()))

          ctx.channel().closeFuture().addListener { f: ChannelFuture =>
            connections.remove(key, f.channel())
            establishedConnections.remove(ctx.channel())
          }

          connectionNegotiated(ctx)
          ctx.fireChannelRead(msg)
        }
      }
    case _ => super.channelRead(ctx, msg)
  }
}

object HandshakeHandler extends ScorexLogging {
  def versionIsSupported(remoteVersion: (Int, Int, Int)): Boolean =
    remoteVersion._1 == 0 && remoteVersion._2 >= 6

  def removeHandshakeHandlers(ctx: ChannelHandlerContext, thisHandler: ChannelHandler): Unit = {
    ctx.pipeline().remove(classOf[HandshakeDecoder])
    ctx.pipeline().remove(classOf[HandshakeTimeoutHandler])
    ctx.pipeline().remove(thisHandler)
  }

  def peerInfo(remoteHandshake: Handshake, channel: Channel): PeerInfo = PeerInfo(
    channel.remoteAddress().asInstanceOf[InetSocketAddress],
    remoteHandshake.declaredAddress,
    remoteHandshake.applicationName,
    remoteHandshake.applicationVersion,
    remoteHandshake.nodeName,
    remoteHandshake.nodeNonce)

  @Sharable
  class Server(
      handshake: Handshake,
      establishedConnections: ConcurrentMap[Channel, PeerInfo],
      connections: ConcurrentMap[PeerKey, Channel],
      peerDatabase: PeerDatabase,
      allChannels: ChannelGroup)
    extends HandshakeHandler(handshake, establishedConnections, connections, peerDatabase) {
    override def connectionNegotiated(ctx: ChannelHandlerContext) = {
      ctx.writeAndFlush(handshake.encode(ctx.alloc().buffer()))
      ctx.channel().closeFuture().addListener((_: ChannelFuture) => allChannels.remove(ctx.channel()))
      allChannels.add(ctx.channel())
    }
  }

  @Sharable
  class Client(
      handshake: Handshake,
      establishedConnections: ConcurrentMap[Channel, PeerInfo],
      connections: ConcurrentMap[PeerKey, Channel],
      peerDatabase: PeerDatabase)
    extends HandshakeHandler(handshake, establishedConnections, connections, peerDatabase) {

    override def connectionNegotiated(ctx: ChannelHandlerContext) = {
      ctx.channel().attr(AttributeKeys.DeclaredAddress).set(ctx.remoteAddress)
    }

    override def channelActive(ctx: ChannelHandlerContext) = {
      ctx.writeAndFlush(handshake.encode(ctx.alloc().buffer()))
      super.channelActive(ctx)
    }
  }
}
