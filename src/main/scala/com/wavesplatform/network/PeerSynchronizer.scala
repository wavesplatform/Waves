package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

@Sharable
class PeerSynchronizer(peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  def requestPeers(ctx: ChannelHandlerContext): Unit = ctx.executor().schedule(10.seconds) {
    if (ctx.channel().isActive) {
      log.trace(s"${id(ctx)} Requesting peers")
      ctx.writeAndFlush(GetPeers)
      requestPeers(ctx)
    }
  }

  override def channelActive(ctx: ChannelHandlerContext) = {
    requestPeers(ctx)
    super.channelActive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case Handshake(_, _, name, nonce, maybeDeclaredAddress) =>
      maybeDeclaredAddress.foreach { declaredAddress =>
        peerDatabase.addPeer(declaredAddress, Some(nonce), Some(name))

        if (sameAddresses(declaredAddress, ctx.channel().remoteAddress())) {
          ctx.channel().attr(AttributeKeys.DeclaredAddress).setIfAbsent(declaredAddress)
        }
      }
      ctx.fireChannelRead(msg)
    case GetPeers =>
      ctx.channel().declaredAddress.foreach(peerDatabase.touch)
      ctx.writeAndFlush(peerDatabase.getKnownPeers.keys.toSeq)
    case KnownPeers(peers) =>
      log.trace(s"${id(ctx)} Got known peers: ${peers.mkString("[", ", ", "]")}")
      ctx.channel().declaredAddress.foreach(peerDatabase.touch)
      peers.foreach(peerDatabase.addPeer(_, None, None))
    case _ =>
      ctx.channel().declaredAddress.foreach(peerDatabase.touch)
      super.channelRead(ctx, msg)
  }
}
