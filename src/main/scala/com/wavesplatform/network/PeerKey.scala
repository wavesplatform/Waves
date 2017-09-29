package com.wavesplatform.network

import java.net.{InetAddress, SocketAddress}

import io.netty.channel.ChannelHandlerContext
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.socket.SocketChannel

sealed trait PeerKey
case object PeerKey {
  case class InetPeerKey(host: InetAddress, nonce: Long) extends PeerKey
  case class SocketPeerKey(host: SocketAddress, nonce: Long) extends PeerKey

  def apply(ctx: ChannelHandlerContext, nodeNonce: Long): PeerKey = ctx.channel() match {
    case x: SocketChannel => PeerKey.InetPeerKey(x.remoteAddress().getAddress, nodeNonce)
    case x: EmbeddedChannel => PeerKey.SocketPeerKey(x.remoteAddress(), nodeNonce)
  }
}
