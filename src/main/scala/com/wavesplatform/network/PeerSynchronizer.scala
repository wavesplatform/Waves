package com.wavesplatform.network

import java.io.IOException
import java.net.InetSocketAddress

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

class PeerSynchronizer(peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var remoteAddress = Option.empty[InetSocketAddress]

  def requestPeers(ctx: ChannelHandlerContext): Unit = ctx.executor().schedule(5.seconds) {
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

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = cause match {
    case ioe: IOException =>
      log.warn(s"${id(ctx)} Exception caught", ioe)
      remoteAddress.foreach(a => peerDatabase.blacklistHost(a.getAddress))
    case _ => super.exceptionCaught(ctx, cause)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case Handshake(_, _, nodeName, nonce, Some(declaredAddress)) =>
      if (sameAddresses(declaredAddress, ctx.channel().remoteAddress())) {
        remoteAddress = Some(declaredAddress)
        peerDatabase.addPeer(declaredAddress, Some(nonce), Some(nodeName))
        peerDatabase.touch(declaredAddress)
      } else {
        log.debug(s"Declared remote address $declaredAddress does not match actual remote address ${ctx.channel().remoteAddress()}")
      }
    case _: Handshake => // peer doesn't declare an address
    case GetPeers =>
      remoteAddress.foreach(peerDatabase.touch)
      ctx.writeAndFlush(peerDatabase.getKnownPeers.keys.toSeq)
    case KnownPeers(peers) =>
      log.trace(s"${id(ctx)} Got known peers: ${peers.mkString("[", ", ", "]")}")
      remoteAddress.foreach(peerDatabase.touch)
      peers.foreach(peerDatabase.addPeer(_, None, None))
    case _ =>
      remoteAddress.foreach(peerDatabase.touch)
      super.channelRead(ctx, msg)
  }
}
