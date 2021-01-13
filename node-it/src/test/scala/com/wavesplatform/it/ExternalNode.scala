package com.wavesplatform.it

import java.net.{InetSocketAddress, URL}

import com.typesafe.config.Config

class ExternalNode(config: Config) extends Node(config) {
  override def nodeExternalPort(internalPort: Int): Int = internalPort

  override def nodeApiEndpoint: URL = new URL(config.getString("node-api-endpoint"))

  override def apiKey: String = config.getString("api-key")

  override def networkAddress: InetSocketAddress = {
    val hostAndPort             = "([^:]+)\\:([\\d+])+".r
    val hostAndPort(host, port) = config.getString("network-address")
    new InetSocketAddress(host, port.toInt)
  }
}
