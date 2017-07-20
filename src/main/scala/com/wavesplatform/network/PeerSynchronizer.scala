package com.wavesplatform.network

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class PeerSynchronizer(peerDatabase: PeerDatabase, peerRequestInterval: FiniteDuration)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private var peersRequested = false

  def requestPeers(ctx: ChannelHandlerContext): Unit = if (ctx.channel().isActive) {
    log.trace(s"${id(ctx)} Requesting peers")
    peersRequested = true
    ctx.writeAndFlush(GetPeers)

    ctx.executor().schedule(peerRequestInterval) {
      requestPeers(ctx)
    }
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = {
    Option(ctx.channel().attr(AttributeKeys.DeclaredAddress).get()).foreach(peerDatabase.touch)
    msg match {
      case hs: Handshake =>
        hs.declaredAddress.foreach(peerDatabase.addCandidate)
        super.channelRead(ctx, msg)
      case GetPeers =>
        ctx.writeAndFlush(KnownPeers(peerDatabase.knownPeers.keys.toSeq))
      case KnownPeers(peers) if peersRequested =>
        peersRequested = false
        log.trace(s"${id(ctx)} Got known peers: ${peers.mkString("[", ", ", "]")}")
        peers.foreach(peerDatabase.addCandidate)
      case KnownPeers(peers) =>
        log.debug(s"${id(ctx)} Got unexpected list of known peers containing ${peers.size} entries")
      case _ =>
        super.channelRead(ctx, msg)
    }
  }
}
