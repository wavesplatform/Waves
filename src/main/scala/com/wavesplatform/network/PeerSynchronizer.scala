package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

@Sharable
class PeerSynchronizer(peerDatabase: PeerDatabase, peerRequestInterval: FiniteDuration) extends ChannelInboundHandlerAdapter
  with ScorexLogging {

  import AttributeKeys.PeersRequested

  def requestPeers(ctx: ChannelHandlerContext): Unit = ctx.executor().schedule(peerRequestInterval) {
    if (ctx.channel().isActive && ctx.channel().attr(PeersRequested).compareAndSet(false, true)) {
      log.trace(s"${id(ctx)} Requesting peers")
      ctx.writeAndFlush(GetPeers)
    } else {
      log.debug(s"${id(ctx)} NOT requesting peers this time, previous request is still pending")
    }
    requestPeers(ctx)
  }

  override def handlerAdded(ctx: ChannelHandlerContext) = {
    ctx.channel().attr(PeersRequested).set(false)
    super.handlerAdded(ctx)
  }

  override def channelActive(ctx: ChannelHandlerContext) = {
    requestPeers(ctx)
    super.channelActive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case hs: Handshake =>
      hs.declaredAddress.foreach { declaredAddress =>
        peerDatabase.addCandidate(declaredAddress)
        ctx.channel().attr(AttributeKeys.DeclaredAddress).setIfAbsent(declaredAddress)
      }
      ctx.fireChannelRead(msg)
    case GetPeers =>
      ctx.channel().declaredAddress.foreach(peerDatabase.touch)
      ctx.writeAndFlush(KnownPeers(peerDatabase.knownPeers.keys.toSeq))
    case KnownPeers(peers) if Option(ctx.channel().attr(PeersRequested).getAndSet(false)).contains(true) =>
      log.trace(s"${id(ctx)} Got known peers: ${peers.mkString("[", ", ", "]")}")
      ctx.channel().declaredAddress.foreach(peerDatabase.touch)
      peers.foreach(peerDatabase.addCandidate)
    case KnownPeers(peers) =>
      log.debug(s"${id(ctx)} Got unexpected list of known peers containing ${peers.size} entries")
    case _ =>
      ctx.channel().declaredAddress.foreach(peerDatabase.touch)
      super.channelRead(ctx, msg)
  }
}
