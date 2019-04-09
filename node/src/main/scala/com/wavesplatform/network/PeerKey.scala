package com.wavesplatform.network

import java.net.{InetAddress, SocketAddress}

import io.netty.channel.ChannelHandlerContext
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.socket.SocketChannel

sealed trait PeerKey
case object PeerKey {
  case class InetPeerKey(host: InetAddress, nonce: Long)     extends PeerKey
  case class SocketPeerKey(host: SocketAddress, nonce: Long) extends PeerKey

  def apply(ctx: ChannelHandlerContext, nodeNonce: Long): Option[PeerKey] = ctx.channel() match {
    case x: SocketChannel   => Option(x.remoteAddress()).map(_.getAddress).map(PeerKey.InetPeerKey(_, nodeNonce))
    case x: EmbeddedChannel => Option(x.remoteAddress()).map(PeerKey.SocketPeerKey(_, nodeNonce))
    case x                  => throw new IllegalArgumentException(s"Can't get PeerKey from ${id(ctx)}, $x")
  }
}
