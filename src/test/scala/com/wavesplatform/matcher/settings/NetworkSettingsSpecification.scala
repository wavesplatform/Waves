package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.NetworkSettings
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class NetworkSettingsSpecification extends FlatSpec with Matchers {

  "NetworkSpecification" should "read values from config" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  logging-level: INFO
        |  network {
        |    file: ${waves.directory}"/peers.dat"
        |    bind-address: "127.0.0.1"
        |    port: 6868
        |    node-name: "default-node-name"
        |    declared-address: "127.0.0.1:6868"
        |    nonce: 0
        |    known-peers = ["8.8.8.8:6868", "4.4.8.8:6868"]
        |    local-only: no
        |    peers-data-residence-time: 1d
        |    black-list-residence-time: 10m
        |    max-connections: 30
        |    connection-timeout: 30s
        |    outbound-buffer-size: 1K
        |    min-ephemeral-port-number: 35368
        |    max-unverified-peers: 0
        |    peers-broadcast-interval: 2m
        |    black-list-threshold: 50
        |    unrequested-packets-threshold: 100
        |    upnp {
        |      enable: yes
        |      gateway-timeout: 10s
        |      discover-timeout: 10s
        |    }
        |  }
        |}
      """.stripMargin).resolve()
    val networkSettings = NetworkSettings.fromConfig(config)

    networkSettings.file should be("/waves/peers.dat")
    networkSettings.bindAddress should be("127.0.0.1")
    networkSettings.port should be(6868)
    networkSettings.nodeName should be("default-node-name")
    networkSettings.declaredAddress should be("127.0.0.1:6868")
    networkSettings.nonce should be(0)
    networkSettings.knownPeers should be(List("8.8.8.8:6868", "4.4.8.8:6868"))
    networkSettings.localOnly should be(false)
    networkSettings.peersDataResidenceTime should be(1.day)
    networkSettings.blackListResidenceTime should be(10.minutes)
    networkSettings.maxConnections should be(30)
    networkSettings.connectionTimeout should be(30.seconds)
    networkSettings.outboundBufferSize should be(1024)
    networkSettings.minEphemeralPortNumber should be(35368)
    networkSettings.maxUnverifiedPeers should be(0)
    networkSettings.peersBroadcastInterval should be(2.minutes)
    networkSettings.blackListThreshold should be(50)
    networkSettings.unrequestedPacketsThreshold should be(100)
    networkSettings.uPnPSettings.enable should be(true)
    networkSettings.uPnPSettings.gatewayTimeout should be(10.seconds)
    networkSettings.uPnPSettings.discoverTimeout should be(10.seconds)
  }

  it should "generate random nonce" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  logging-level: INFO
        |  network {
        |    file: ${waves.directory}"/peers.dat"
        |    bind-address: "127.0.0.1"
        |    port: 6868
        |    node-name: "default-node-name"
        |    declared-address: "127.0.0.1:6868"
        |    known-peers = ["8.8.8.8:6868", "4.4.8.8:6868"]
        |    local-only: no
        |    peers-data-residence-time: 1d
        |    black-list-residence-time: 10m
        |    max-connections: 30
        |    connection-timeout: 30s
        |    outbound-buffer-size: 1K
        |    min-ephemeral-port-number: 35368
        |    max-unverified-peers: 0
        |    peers-broadcast-interval: 2m
        |    black-list-threshold: 50
        |    unrequested-packets-threshold: 100
        |    upnp {
        |      enable: yes
        |      gateway-timeout: 10s
        |      discover-timeout: 10s
        |    }
        |  }
        |}
      """.stripMargin).resolve()
    val networkSettings = NetworkSettings.fromConfig(config)

    networkSettings.nonce should not be 0
  }

  it should "build node name using nonce" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  logging-level: INFO
        |  network {
        |    file: ${waves.directory}"/peers.dat"
        |    bind-address: "127.0.0.1"
        |    port: 6868
        |    nonce: 12345
        |    declared-address: "127.0.0.1:6868"
        |    known-peers = ["8.8.8.8:6868", "4.4.8.8:6868"]
        |    local-only: no
        |    peers-data-residence-time: 1d
        |    black-list-residence-time: 10m
        |    max-connections: 30
        |    connection-timeout: 30s
        |    outbound-buffer-size: 1K
        |    min-ephemeral-port-number: 35368
        |    max-unverified-peers: 0
        |    peers-broadcast-interval: 2m
        |    black-list-threshold: 50
        |    unrequested-packets-threshold: 100
        |    upnp {
        |      enable: yes
        |      gateway-timeout: 10s
        |      discover-timeout: 10s
        |    }
        |  }
        |}
      """.stripMargin).resolve()
    val networkSettings = NetworkSettings.fromConfig(config)

    networkSettings.nonce should be(12345)
    networkSettings.nodeName should be("Node-12345")
  }


  it should "build node name using random nonce" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  logging-level: INFO
        |  network {
        |    file: ${waves.directory}"/peers.dat"
        |    bind-address: "127.0.0.1"
        |    port: 6868
        |    declared-address: "127.0.0.1:6868"
        |    known-peers = ["8.8.8.8:6868", "4.4.8.8:6868"]
        |    local-only: no
        |    peers-data-residence-time: 1d
        |    black-list-residence-time: 10m
        |    max-connections: 30
        |    connection-timeout: 30s
        |    outbound-buffer-size: 1K
        |    min-ephemeral-port-number: 35368
        |    max-unverified-peers: 0
        |    peers-broadcast-interval: 2m
        |    black-list-threshold: 50
        |    unrequested-packets-threshold: 100
        |    upnp {
        |      enable: yes
        |      gateway-timeout: 10s
        |      discover-timeout: 10s
        |    }
        |  }
        |}
      """.stripMargin).resolve()
    val networkSettings = NetworkSettings.fromConfig(config)

    networkSettings.nonce should not be 0
    networkSettings.nodeName should be(s"Node-${networkSettings.nonce}")
  }
}
