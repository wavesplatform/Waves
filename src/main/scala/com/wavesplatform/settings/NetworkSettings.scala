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
                           blockchainReadinessCacheTime: FiniteDuration,
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

    NetworkSettings(file = file,
      bindAddress = bindAddress,
      declaredAddress = declaredAddress,
      nodeName = nodeName,
      nonce = nonce,
      knownPeers = config.as[Seq[String]]("known-peers"),
      peersDataResidenceTime = config.as[FiniteDuration]("peers-data-residence-time"),
      blackListResidenceTime = config.as[FiniteDuration]("black-list-residence-time"),
      blockchainReadinessCacheTime = config.as[FiniteDuration]("blockchain-readiness-cache-time"),
      maxInboundConnections = config.as[Int]("max-inbound-connections"),
      maxOutboundConnections = config.as[Int]("max-outbound-connections"),
      maxConnectionsPerHost = config.as[Int]("max-single-host-connections"),
      connectionTimeout = config.as[FiniteDuration]("connection-timeout"),
      outboundBufferSize = config.getBytes("outbound-buffer-size"),
      maxUnverifiedPeers = config.as[Int]("max-unverified-peers"),
      peersBroadcastInterval = config.as[FiniteDuration]("peers-broadcast-interval"),
      handshakeTimeout = config.as[FiniteDuration]("handshake-timeout"),
      uPnPSettings = config.as[UPnPSettings]("upnp"))
  }

  private def randomNonce: Long = {
    val base = 1000

    (Random.nextInt(base) + base) * Random.nextInt(base) + Random.nextInt(base)
  }
}