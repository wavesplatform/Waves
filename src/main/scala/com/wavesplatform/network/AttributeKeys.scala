package com.wavesplatform.network

import java.net.InetSocketAddress

import io.netty.util.AttributeKey

object AttributeKeys {
  val NodeName = AttributeKey.newInstance[String]("node-name")
  val DeclaredAddress = AttributeKey.newInstance[InetSocketAddress]("declared-address")
}
