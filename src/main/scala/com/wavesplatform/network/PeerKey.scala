package com.wavesplatform.network

import java.net.{InetAddress, SocketAddress}

sealed trait PeerKey
case object PeerKey {
  case class InetPeerKey(host: InetAddress, nonce: Long) extends PeerKey
  case class SocketPeerKey(host: SocketAddress, nonce: Long) extends PeerKey
}
