package com.wavesplatform.network

import java.net.InetSocketAddress

case class PeerInfo(
    remoteAddress: InetSocketAddress,
    declaredAddress: Option[InetSocketAddress],
    applicationName: String,
    applicationVersion: (Int, Int, Int),
    nodeName: String,
    nodeNonce: Long)
