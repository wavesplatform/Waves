package com.wavesplatform.network

import java.util
import java.util.concurrent.{ConcurrentMap, TimeUnit}

import com.wavesplatform.network.Handshake.InvalidHandshakeException
import com.wavesplatform.utils.ScorexLogging
import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.handler.codec.ReplayingDecoder
import io.netty.util.AttributeKey
import io.netty.util.concurrent.ScheduledFuture

import scala.concurrent.duration.FiniteDuration

class HandshakeDecoder(peerDatabase: PeerDatabase) extends ReplayingDecoder[Void] with ScorexLogging {
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]): Unit =
    try {
      out.add(Handshake.decode(in))
      ctx.pipeline().remove(this)
    } catch {
      case e: InvalidHandshakeException => block(ctx, e)
    }

  protected def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
    peerDatabase.blacklistAndClose(ctx.channel(), e.getMessage)
  }
}

case object HandshakeTimeoutExpired

class HandshakeTimeoutHandler(handshakeTimeout: FiniteDuration) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var timeout: Option[ScheduledFuture[_]] = None

  private def cancelTimeout(): Unit = timeout.foreach(_.cancel(true))

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    log.trace(s"${id(ctx)} Scheduling handshake timeout, timeout = $handshakeTimeout")
    timeout = Some(
      ctx
        .channel()
        .eventLoop()
        .schedule(
          { () =>
            log.trace(s"${id(ctx)} Firing handshake timeout expired")
            ctx.fireChannelRead(HandshakeTimeoutExpired)
          },
          handshakeTimeout.toMillis,
          TimeUnit.MILLISECONDS
        ))

    super.channelActive(ctx)
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    cancelTimeout()
    super.channelInactive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case hs: Handshake =>
      cancelTimeout()
      super.channelRead(ctx, hs)
    case other =>
      super.channelRead(ctx, other)
  }
}

abstract class HandshakeHandler(localHandshake: Handshake,
                                establishedConnections: ConcurrentMap[Channel, PeerInfo],
                                peerConnections: ConcurrentMap[PeerKey, Channel],
                                peerDatabase: PeerDatabase,
                                allChannels: ChannelGroup)
    extends ChannelInboundHandlerAdapter
    with ScorexLogging {

  import HandshakeHandler._

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case HandshakeTimeoutExpired =>
      peerDatabase.blacklistAndClose(ctx.channel(), "Timeout expired while waiting for handshake")
    case remoteHandshake: Handshake =>
      if (localHandshake.applicationName != remoteHandshake.applicationName)
        peerDatabase.blacklistAndClose(
          ctx.channel(),
          s"Remote application name ${remoteHandshake.applicationName} does not match local ${localHandshake.applicationName}")
      else if (!versionIsSupported(remoteHandshake.applicationVersion))
        peerDatabase.blacklistAndClose(ctx.channel(), s"Remote application version ${remoteHandshake.applicationVersion} is not supported")
      else {
        PeerKey(ctx, remoteHandshake.nodeNonce) match {
          case None =>
            log.warn(s"Can't get PeerKey from ${id(ctx)}")
            ctx.close()

          case Some(key) =>
            val previousPeer = peerConnections.putIfAbsent(key, ctx.channel())
            if (previousPeer == null) {
              log.info(s"${id(ctx)} Accepted handshake $remoteHandshake")
              removeHandshakeHandlers(ctx, this)
              establishedConnections.put(ctx.channel(), peerInfo(remoteHandshake, ctx.channel()))

              ctx.channel().attr(NodeNameAttributeKey).set(remoteHandshake.nodeName)
              ctx.channel().closeFuture().addListener { f: ChannelFuture =>
                peerConnections.remove(key, f.channel())
                establishedConnections.remove(f.channel())
                log.trace(s"${id(f.channel())} was closed")
              }

              connectionNegotiated(ctx)
              ctx.fireChannelRead(msg)
            } else {
              val peerAddress = ctx.remoteAddress.getOrElse("unknown")
              log.debug(s"${id(ctx)} Already connected to peer $peerAddress with nonce ${remoteHandshake.nodeNonce} on channel ${id(previousPeer)}")
              ctx.close()
            }
        }
      }
    case _ => super.channelRead(ctx, msg)
  }

  protected def connectionNegotiated(ctx: ChannelHandlerContext): Unit = {
    ctx.channel().closeFuture().addListener((_: ChannelFuture) => allChannels.remove(ctx.channel()))
    allChannels.add(ctx.channel())
  }

  protected def sendLocalHandshake(ctx: ChannelHandlerContext): Unit = {
    ctx.writeAndFlush(localHandshake.encode(ctx.alloc().buffer()))
  }
}

object HandshakeHandler extends ScorexLogging {

  val NodeNameAttributeKey = AttributeKey.newInstance[String]("name")

  def versionIsSupported(remoteVersion: (Int, Int, Int)): Boolean =
    (remoteVersion._1 == 0 && remoteVersion._2 >= 13) || (remoteVersion._1 == 1 && remoteVersion._2 >= 0)

  def removeHandshakeHandlers(ctx: ChannelHandlerContext, thisHandler: ChannelHandler): Unit = {
    ctx.pipeline().remove(classOf[HandshakeTimeoutHandler])
    ctx.pipeline().remove(thisHandler)
  }

  def peerInfo(remoteHandshake: Handshake, channel: Channel): PeerInfo =
    PeerInfo(
      channel.remoteAddress(),
      remoteHandshake.declaredAddress,
      remoteHandshake.applicationName,
      remoteHandshake.applicationVersion,
      remoteHandshake.nodeName,
      remoteHandshake.nodeNonce
    )

  @Sharable
  class Server(handshake: Handshake,
               establishedConnections: ConcurrentMap[Channel, PeerInfo],
               peerConnections: ConcurrentMap[PeerKey, Channel],
               peerDatabase: PeerDatabase,
               allChannels: ChannelGroup)
      extends HandshakeHandler(handshake, establishedConnections, peerConnections, peerDatabase, allChannels) {
    override protected def connectionNegotiated(ctx: ChannelHandlerContext): Unit = {
      sendLocalHandshake(ctx)
      super.connectionNegotiated(ctx)
    }
  }

  @Sharable
  class Client(handshake: Handshake,
               establishedConnections: ConcurrentMap[Channel, PeerInfo],
               peerConnections: ConcurrentMap[PeerKey, Channel],
               peerDatabase: PeerDatabase,
               allChannels: ChannelGroup)
      extends HandshakeHandler(handshake, establishedConnections, peerConnections, peerDatabase, allChannels) {
    override protected def channelActive(ctx: ChannelHandlerContext): Unit = {
      sendLocalHandshake(ctx)
      super.channelActive(ctx)
    }
  }

}
