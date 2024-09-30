package com.wavesplatform.settings

import java.net.InetSocketAddress

import com.typesafe.config.ConfigFactory
import com.wavesplatform.test.FlatSpec
import net.ceedubs.ficus.Ficus.*

import scala.concurrent.duration.*

class NetworkSettingsSpecification extends FlatSpec {

  "NetworkSpecification" should "read values from config" in {
    val config = loadConfig(ConfigFactory.parseString("""waves.network {
                                                        |  bind-address: "127.0.0.1"
                                                        |  port: 6868
                                                        |  node-name: "default-node-name"
                                                        |  declared-address: "127.0.0.1:6868"
                                                        |  nonce: 0
                                                        |  known-peers = ["8.8.8.8:6868", "4.4.8.8:6868"]
                                                        |  local-only: no
                                                        |  peers-data-residence-time: 1d
                                                        |  black-list-residence-time: 10m
                                                        |  break-idle-connections-timeout: 53s
                                                        |  max-inbound-connections: 30
                                                        |  max-outbound-connections = 20
                                                        |  max-single-host-connections = 2
                                                        |  connection-timeout: 30s
                                                        |  max-unverified-peers: 0
                                                        |  peers-broadcast-interval: 2m
                                                        |  black-list-threshold: 50
                                                        |  unrequested-packets-threshold: 100
                                                        |  upnp {
                                                        |    enable: yes
                                                        |    gateway-timeout: 10s
                                                        |    discover-timeout: 10s
                                                        |  }
                                                        |  traffic-logger {
                                                        |    ignore-tx-messages = [28]
                                                        |    ignore-rx-messages = [23]
                                                        |  }
                                                        |}""".stripMargin))
    val networkSettings = config.as[NetworkSettings]("waves.network")

    networkSettings.bindAddress should be(Some(new InetSocketAddress("127.0.0.1", 6868)))
    networkSettings.nodeName should be("default-node-name")
    networkSettings.declaredAddress should be(Some(new InetSocketAddress("127.0.0.1", 6868)))
    networkSettings.nonce should be(0)
    networkSettings.knownPeers should be(List("8.8.8.8:6868", "4.4.8.8:6868"))
    networkSettings.peersDataResidenceTime should be(1.day)
    networkSettings.blackListResidenceTime should be(10.minutes)
    networkSettings.breakIdleConnectionsTimeout should be(53.seconds)
    networkSettings.maxInboundConnections should be(30)
    networkSettings.maxOutboundConnections should be(20)
    networkSettings.maxConnectionsPerHost should be(2)
    networkSettings.connectionTimeout should be(30.seconds)
    networkSettings.maxUnverifiedPeers should be(0)
    networkSettings.peersBroadcastInterval should be(2.minutes)
    networkSettings.uPnPSettings.enable should be(true)
    networkSettings.uPnPSettings.gatewayTimeout should be(10.seconds)
    networkSettings.uPnPSettings.discoverTimeout should be(10.seconds)
    networkSettings.trafficLogger.ignoreTxMessages should be(Set(28))
    networkSettings.trafficLogger.ignoreRxMessages should be(Set(23))
  }

  it should "generate random nonce" in {
    val config          = loadConfig(ConfigFactory.empty())
    val networkSettings = config.as[NetworkSettings]("waves.network")

    networkSettings.nonce should not be 0
  }

  it should "build node name using nonce" in {
    val config          = loadConfig(ConfigFactory.parseString("waves.network.nonce = 12345"))
    val networkSettings = config.as[NetworkSettings]("waves.network")

    networkSettings.nonce should be(12345)
    networkSettings.nodeName should be("Node-12345")
  }

  it should "build node name using random nonce" in {
    val config          = loadConfig(ConfigFactory.empty())
    val networkSettings = config.as[NetworkSettings]("waves.network")

    networkSettings.nonce should not be 0
    networkSettings.nodeName should be(s"Node-${networkSettings.nonce}")
  }

  it should "fail with IllegalArgumentException on too long node name" in {
    val config = loadConfig(
      ConfigFactory.parseString(
        "waves.network.node-name = очень-длинное-название-в-многобайтной-кодировке-отличной-от-однобайтной-кодировки-американского-института-стандартов"
      )
    )
    intercept[IllegalArgumentException] {
      config.as[NetworkSettings]("waves.network")
    }
  }
}
