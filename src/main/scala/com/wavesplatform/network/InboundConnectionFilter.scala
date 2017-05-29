package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelFuture, ChannelHandlerContext}
import io.netty.handler.ipfilter.AbstractRemoteAddressFilter
import scorex.utils.ScorexLogging

@Sharable
class InboundConnectionFilter(peerDatabase: PeerDatabase, maxInboundConnections: Int, maxConnectionsPerHost: Int)
  extends AbstractRemoteAddressFilter[InetSocketAddress] with ScorexLogging {
  private val inboundConnectionCount = new AtomicInteger(0)
  private val perHostConnectionCount = new ConcurrentHashMap[InetAddress, Int]

  private def dec(remoteAddress: InetAddress) = {
    inboundConnectionCount.decrementAndGet()
    perHostConnectionCount.compute(remoteAddress, (_, cnt) => cnt - 1)
    null.asInstanceOf[ChannelFuture]
  }

  override def accept(ctx: ChannelHandlerContext, remoteAddress: InetSocketAddress) = {
    val newTotal = inboundConnectionCount.incrementAndGet()
    val newCount = perHostConnectionCount.compute(remoteAddress.getAddress, (_, cnt) => Option(cnt).fold(1)(_ + 1))

    newTotal <= maxInboundConnections &&
      newCount <= maxConnectionsPerHost &&
      peerDatabase.getBlacklist.contains(remoteAddress.getHostName)
  }

  override def channelAccepted(ctx: ChannelHandlerContext, remoteAddress: InetSocketAddress) =
    ctx.channel().closeFuture().addListener((_: ChannelFuture) => dec(remoteAddress.getAddress))

  override def channelRejected(ctx: ChannelHandlerContext, remoteAddress: InetSocketAddress) =
    dec(remoteAddress.getAddress)
}
