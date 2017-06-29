package com.wavesplatform.settings

import java.io.File
import java.net.{InetSocketAddress, URI}

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

case class UPnPSettings(enable: Boolean, gatewayTimeout: FiniteDuration, discoverTimeout: FiniteDuration)

case class NetworkSettings(file: Option[File],
                           bindAddress: InetSocketAddress,
                           declaredAddress: Option[InetSocketAddress],
                           nodeName: String,
                           nonce: Long,
                           knownPeers: Seq[String],
                           peersDataResidenceTime: FiniteDuration,
                           blackListResidenceTime: FiniteDuration,
                           maxInboundConnections: Int,
                           maxOutboundConnections: Int,
                           maxConnectionsPerHost: Int,
                           connectionTimeout: FiniteDuration,
                           outboundBufferSize: Long,
                           maxUnverifiedPeers: Int,
                           peersBroadcastInterval: FiniteDuration,
                           handshakeTimeout: FiniteDuration,
                           uPnPSettings: UPnPSettings)

object NetworkSettings {
  implicit val networkSettingsValueReader: ValueReader[NetworkSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private def fromConfig(config: Config): NetworkSettings = {
    val file = config.getAs[File]("file")
    val bindAddress = new InetSocketAddress(config.as[String]("bind-address"), config.as[Int]("port"))
    val nonce = config.getOrElse("nonce", randomNonce)
    val nodeName = config.getOrElse("node-name", s"Node-$nonce")
    val declaredAddress = config.getAs[String]("declared-address").map { address =>
      val uri = new URI(s"my://$address")
      new InetSocketAddress(uri.getHost, uri.getPort)
    }

    val knownPeers = config.as[Seq[String]]("known-peers")
    val peersDataResidenceTime = config.as[FiniteDuration]("peers-data-residence-time")
    val blackListResidenceTime = config.as[FiniteDuration]("black-list-residence-time")
    val maxInboundConnections = config.as[Int]("max-inbound-connections")
    val maxOutboundConnections = config.as[Int]("max-outbound-connections")
    val maxConnectionsFromSingleHost = config.as[Int]("max-single-host-connections")
    val connectionTimeout = config.as[FiniteDuration]("connection-timeout")
    val outboundBufferSize = config.getBytes("outbound-buffer-size")
    val maxUnverifiedPeers = config.as[Int]("max-unverified-peers")
    val peersBroadcastInterval = config.as[FiniteDuration]("peers-broadcast-interval")
    val handshakeTimeout = config.as[FiniteDuration]("handshake-timeout")
    val uPnPSettings = config.as[UPnPSettings]("upnp")

    NetworkSettings(file, bindAddress, declaredAddress, nodeName, nonce, knownPeers,
      peersDataResidenceTime, blackListResidenceTime, maxInboundConnections, maxOutboundConnections,
      maxConnectionsFromSingleHost, connectionTimeout, outboundBufferSize, maxUnverifiedPeers,
      peersBroadcastInterval, handshakeTimeout, uPnPSettings)
  }

  private def randomNonce: Long = {
    val base = 1000

    (Random.nextInt(base) + base) * Random.nextInt(base) + Random.nextInt(base)
  }
}