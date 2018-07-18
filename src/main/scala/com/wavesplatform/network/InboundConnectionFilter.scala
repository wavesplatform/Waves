package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelFuture, ChannelHandlerContext}
import io.netty.handler.ipfilter.AbstractRemoteAddressFilter

@Sharable
class InboundConnectionFilter(peerDatabase: PeerDatabase, maxInboundConnections: Int, maxConnectionsPerHost: Int)
    extends AbstractRemoteAddressFilter[InetSocketAddress]
    with ScorexLogging {
  private val inboundConnectionCount = new AtomicInteger(0)
  private val perHostConnectionCount = new ConcurrentHashMap[InetAddress, Int]
  private val emptyChannelFuture     = null.asInstanceOf[ChannelFuture]

  private def dec(remoteAddress: InetAddress) = {
    inboundConnectionCount.decrementAndGet()
    log.trace(s"Number of inbound connections: ${inboundConnectionCount.get()}")
    perHostConnectionCount.compute(remoteAddress, (_, cnt) => cnt - 1)
    emptyChannelFuture
  }

  override def accept(ctx: ChannelHandlerContext, remoteAddress: InetSocketAddress): Boolean = Option(remoteAddress.getAddress) match {
    case None =>
      log.debug(s"Can't obtain an address from $remoteAddress")
      false

    case Some(address) =>
      val newTotal        = inboundConnectionCount.incrementAndGet()
      val newCountPerHost = perHostConnectionCount.compute(address, (_, cnt) => Option(cnt).fold(1)(_ + 1))
      val isBlacklisted   = peerDatabase.blacklistedHosts.contains(address)

      val accepted = newTotal <= maxInboundConnections &&
        newCountPerHost <= maxConnectionsPerHost &&
        !isBlacklisted

      log.trace(
        s"Check inbound connection from $remoteAddress: new inbound total = $newTotal, " +
          s"connections with this host = $newCountPerHost, address ${if (isBlacklisted) "IS" else "is not"} blacklisted, " +
          s"${if (accepted) "is" else "is not"} accepted"
      )

      accepted
  }

  override def channelAccepted(ctx: ChannelHandlerContext, remoteAddress: InetSocketAddress): Unit =
    ctx.channel().closeFuture().addListener((_: ChannelFuture) => Option(remoteAddress.getAddress).foreach(dec))

  override def channelRejected(ctx: ChannelHandlerContext, remoteAddress: InetSocketAddress): ChannelFuture =
    Option(remoteAddress.getAddress).fold(emptyChannelFuture)(dec)
}
