package com.wavesplatform.network

import java.net.InetSocketAddress

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

class PeerSynchronizer(peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var touchTask = Option.empty[ScheduledFuture[_]]
  private var requestPeersTask = Option.empty[ScheduledFuture[_]]

  override def channelActive(ctx: ChannelHandlerContext) = {
    requestPeersTask = Some(ctx.executor().scheduleWithFixedDelay(1.second, 5.seconds) {
      ctx.writeAndFlush(GetPeers)
    })
    super.channelActive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case Handshake(_, _, nodeName, nonce, Some(declaredAddress)) =>
      if (sameAddresses(declaredAddress, ctx.channel().remoteAddress())) {
        peerDatabase.addPeer(declaredAddress, Some(nonce), Some(nodeName))
        touchTask = Some(ctx.executor().scheduleWithFixedDelay(20.seconds, 20.seconds) {
          if (ctx.channel().isActive) {
            peerDatabase.touch(ctx.channel().localAddress().asInstanceOf[InetSocketAddress])
          }
        })
      } else {
        log.debug(s"Declared remote address $declaredAddress does not match actual remote address ${ctx.channel().remoteAddress()}")
      }

    case GetPeers => ctx.writeAndFlush(peerDatabase.getKnownPeers.keys.toSeq)
    case KnownPeers(peers) => peers.foreach(peerDatabase.addPeer(_, None, None))

    case _ => super.channelRead(ctx, msg)
  }

  override def channelInactive(ctx: ChannelHandlerContext) = {
    touchTask.foreach(_.cancel(false))
    requestPeersTask.foreach(_.cancel(false))
    super.channelInactive(ctx)
  }
}
