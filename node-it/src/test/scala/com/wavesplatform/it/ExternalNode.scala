package com.wavesplatform.it

import com.typesafe.config.Config

import java.net.{InetSocketAddress, URI, URL}

class ExternalNode(config: Config) extends Node(config) {
  override def nodeExternalPort(internalPort: Int): Int = internalPort

  override def nodeApiEndpoint: URL = URI.create(config.getString("node-api-endpoint")).toURL

  override def apiKey: String = config.getString("api-key")

  override def networkAddress: InetSocketAddress = {
    val hostAndPort             = "([^:]+):([\\d+])+".r
    val hostAndPort(host, port) = (config.getString("network-address"): @unchecked)
    new InetSocketAddress(host, port.toInt)
  }

  override def networkAddressAccessibleFromHost: InetSocketAddress = networkAddress
}
