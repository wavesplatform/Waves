package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

case class UPnPSettings(enable: Boolean, gatewayTimeout: FiniteDuration, discoverTimeout: FiniteDuration)

object UPnPSettings {
  val configPath: String = "waves.network.upnp"

  def fromConfig(config: Config): UPnPSettings = {
    val enable = config.as[Boolean](s"$configPath.enable")
    val gatewayTimeout = config.as[FiniteDuration](s"$configPath.gateway-timeout")
    val discoverTimeout = config.as[FiniteDuration](s"$configPath.discover-timeout")

    UPnPSettings(enable, gatewayTimeout, discoverTimeout)
  }
}

case class NetworkSettings(file: String,
                           bindAddress: String,
                           port: Int,
                           nodeName: String,
                           declaredAddress: String,
                           nonce: Long,
                           knownPeers: List[String],
                           localOnly: Boolean,
                           peersDataResidenceTime: FiniteDuration,
                           blackListResidenceTime: FiniteDuration,
                           maxConnections: Int,
                           connectionTimeout: FiniteDuration,
                           outboundBufferSize: Long,
                           minEphemeralPortNumber: Int,
                           maxUnverifiedPeers: Int,
                           peersBroadcastInterval: FiniteDuration,
                           blackListThreshold: Int,
                           unrequestedPacketsThreshold: Int,
                           uPnPSettings: UPnPSettings)

object NetworkSettings {
  val configPath: String = "waves.network"

  def fromConfig(config: Config): NetworkSettings = {
    val file = config.as[String](s"$configPath.file")
    val bindAddress = config.as[String](s"$configPath.bind-address")
    val port = config.as[Int](s"$configPath.port")
    val nonce = if (config.hasPath(s"$configPath.nonce")) config.as[Long](s"$configPath.nonce") else randomNonce
    val nodeName = if (config.hasPath(s"$configPath.node-name")) config.as[String](s"$configPath.node-name") else s"Node-$nonce"
    val declaredAddress = config.as[String](s"$configPath.declared-address")
    val knownPeers = config.as[List[String]](s"$configPath.known-peers")
    val localOnly = config.as[Boolean](s"$configPath.local-only")
    val peersDataResidenceTime = config.as[FiniteDuration](s"$configPath.peers-data-residence-time")
    val blackListResidenceTime = config.as[FiniteDuration](s"$configPath.black-list-residence-time")
    val maxConnections = config.as[Int](s"$configPath.max-connections")
    val connectionTimeout = config.as[FiniteDuration](s"$configPath.connection-timeout")
    val outboundBufferSize = config.getBytes(s"$configPath.outbound-buffer-size")
    val minEphemeralPortNumber = config.as[Int](s"$configPath.min-ephemeral-port-number")
    val maxUnverifiedPeers = config.as[Int](s"$configPath.max-unverified-peers")
    val peersBroadcastInterval = config.as[FiniteDuration](s"$configPath.peers-broadcast-interval")
    val blackListThreshold = config.as[Int](s"$configPath.black-list-threshold")
    val unrequestedPacketsThreshold = config.as[Int](s"$configPath.unrequested-packets-threshold")
    val uPnPSettings = UPnPSettings.fromConfig(config)

    NetworkSettings(file, bindAddress, port, nodeName, declaredAddress, nonce, knownPeers, localOnly,
      peersDataResidenceTime, blackListResidenceTime, maxConnections, connectionTimeout, outboundBufferSize,
      minEphemeralPortNumber, maxUnverifiedPeers, peersBroadcastInterval, blackListThreshold,
      unrequestedPacketsThreshold, uPnPSettings)
  }

  private def randomNonce: Long = {
    val base = 1000

    (Random.nextInt(base) + base) * Random.nextInt(base) + Random.nextInt(base)
  }
}