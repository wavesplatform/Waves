package com.wavesplatform.network

import java.net.InetSocketAddress

case class PeerInfo(
    remoteAddress: InetSocketAddress,
    nonce: Long,
    declaredAddress: Option[InetSocketAddress])
