package com.wavesplatform.it.network.client

import java.net.InetSocketAddress

import io.netty.util.AttributeKey

object AttributeKeys {
  val NodeName = AttributeKey.newInstance[String]("node-name")
  val RemoteAddress = AttributeKey.newInstance[InetSocketAddress]("remote-address")
}
