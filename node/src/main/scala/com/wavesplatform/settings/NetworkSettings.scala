package com.wavesplatform.settings

import com.wavesplatform.network.TrafficLogger
import com.wavesplatform.utils.*
import pureconfig.*
import pureconfig.generic.auto.*

import java.io.File
import java.net.{InetSocketAddress, URI}
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

case class UPnPSettings(enable: Boolean, gatewayTimeout: FiniteDuration, discoverTimeout: FiniteDuration)

case class NetworkSettingsDto(
    file: Option[File],
    bindAddress: Option[String],
    port: Option[Int],
    declaredAddress: Option[String],
    nodeName: Option[String],
    nonce: Option[Long],
    knownPeers: Seq[String],
    peersDataResidenceTime: FiniteDuration,
    blackListResidenceTime: FiniteDuration,
    breakIdleConnectionsTimeout: FiniteDuration,
    maxInboundConnections: Int,
    maxOutboundConnections: Int,
    maxSingleHostConnections: Int,
    minConnections: Option[Int],
    connectionTimeout: FiniteDuration,
    maxUnverifiedPeers: Int,
    enablePeersExchange: Boolean,
    enableBlacklisting: Boolean,
    peersBroadcastInterval: FiniteDuration,
    handshakeTimeout: FiniteDuration,
    suspensionResidenceTime: FiniteDuration,
    receivedTxsCacheTimeout: FiniteDuration,
    upnp: UPnPSettings,
    trafficLogger: TrafficLogger.Settings
) {
  def toNetworkSettings: NetworkSettings = {
    def randomNonce: Long = {
      val base = 1000
      (Random.nextInt(base) + base) * Random.nextInt(base) + Random.nextInt(base)
    }
    val MaxNodeNameBytesLength = 127

    val declaredAddress1 = declaredAddress.map { address =>
      val uri = new URI(s"my://$address")
      new InetSocketAddress(uri.getHost, uri.getPort)
    }
    val nonce1    = nonce.getOrElse(randomNonce)
    val nodeName1 = nodeName.getOrElse(s"Node-$nonce1")
    require(nodeName1.utf8Bytes.length <= MaxNodeNameBytesLength, s"Node name should have length less than $MaxNodeNameBytesLength bytes")
    val bindAddress1 = for {
      addr <- bindAddress
      p    <- port
    } yield new InetSocketAddress(addr, p)

    NetworkSettings(
      file = file,
      bindAddress = bindAddress1,
      declaredAddress = declaredAddress1,
      nodeName = nodeName1,
      nonce = nonce1,
      knownPeers = knownPeers,
      peersDataResidenceTime = peersDataResidenceTime,
      blackListResidenceTime = blackListResidenceTime,
      breakIdleConnectionsTimeout = breakIdleConnectionsTimeout,
      maxInboundConnections = maxInboundConnections,
      maxOutboundConnections = maxOutboundConnections,
      maxConnectionsPerHost = maxSingleHostConnections,
      minConnections = minConnections,
      connectionTimeout = connectionTimeout,
      maxUnverifiedPeers = maxUnverifiedPeers,
      enablePeersExchange = enablePeersExchange,
      enableBlacklisting = enableBlacklisting,
      peersBroadcastInterval = peersBroadcastInterval,
      handshakeTimeout = handshakeTimeout,
      suspensionResidenceTime = suspensionResidenceTime,
      receivedTxsCacheTimeout = receivedTxsCacheTimeout,
      uPnPSettings = upnp,
      trafficLogger = trafficLogger
    )
  }
}

case class NetworkSettings(
    file: Option[File],
    bindAddress: Option[InetSocketAddress],
    declaredAddress: Option[InetSocketAddress],
    nodeName: String,
    nonce: Long,
    knownPeers: Seq[String],
    peersDataResidenceTime: FiniteDuration,
    blackListResidenceTime: FiniteDuration,
    breakIdleConnectionsTimeout: FiniteDuration,
    maxInboundConnections: Int,
    maxOutboundConnections: Int,
    maxConnectionsPerHost: Int,
    minConnections: Option[Int],
    connectionTimeout: FiniteDuration,
    maxUnverifiedPeers: Int,
    enablePeersExchange: Boolean,
    enableBlacklisting: Boolean,
    peersBroadcastInterval: FiniteDuration,
    handshakeTimeout: FiniteDuration,
    suspensionResidenceTime: FiniteDuration,
    receivedTxsCacheTimeout: FiniteDuration,
    uPnPSettings: UPnPSettings,
    trafficLogger: TrafficLogger.Settings
)

object NetworkSettings {
  implicit val configReader: ConfigReader[NetworkSettings] = ConfigReader[NetworkSettingsDto].map(_.toNetworkSettings)
}
