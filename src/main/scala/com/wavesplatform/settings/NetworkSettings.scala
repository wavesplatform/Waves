package com.wavesplatform.settings

import java.io.File
import java.net.{InetSocketAddress, URI}

import com.google.common.base.Charsets
import com.typesafe.config.{Config, ConfigException}
import com.wavesplatform.network.TrafficLogger
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
                           breakIdleConnectionsTimeout: FiniteDuration,
                           maxInboundConnections: Int,
                           maxOutboundConnections: Int,
                           maxConnectionsPerHost: Int,
                           connectionTimeout: FiniteDuration,
                           maxUnverifiedPeers: Int,
                           enablePeersExchange: Boolean,
                           enableBlacklisting: Boolean,
                           peersBroadcastInterval: FiniteDuration,
                           handshakeTimeout: FiniteDuration,
                           suspensionResidenceTime: FiniteDuration,
                           receivedTxsCacheTimeout: FiniteDuration,
                           uPnPSettings: UPnPSettings,
                           trafficLogger: TrafficLogger.Settings)

object NetworkSettings {
  private val MaxNodeNameBytesLength = 127

  implicit val valueReader: ValueReader[NetworkSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private[this] def fromConfig(config: Config): NetworkSettings = {
    implicit val _: ValueReader[Byte] = { (cfg: Config, path: String) =>
      val x = cfg.getInt(path)
      if (x.isValidByte) x.toByte
      else throw new ConfigException.WrongType(config.origin(), s"$path has an invalid value: '$x' expected to be a byte")
    }

    val file        = config.getAs[File]("file")
    val bindAddress = new InetSocketAddress(config.as[String]("bind-address"), config.as[Int]("port"))
    val nonce       = config.getOrElse("nonce", randomNonce)
    val nodeName    = config.getOrElse("node-name", s"Node-$nonce")
    require(nodeName.getBytes(Charsets.UTF_8).length <= MaxNodeNameBytesLength,
            s"Node name should have length less than $MaxNodeNameBytesLength bytes")
    val declaredAddress = config.getAs[String]("declared-address").map { address =>
      val uri = new URI(s"my://$address")
      new InetSocketAddress(uri.getHost, uri.getPort)
    }

    val knownPeers                   = config.as[Seq[String]]("known-peers")
    val peersDataResidenceTime       = config.as[FiniteDuration]("peers-data-residence-time")
    val blackListResidenceTime       = config.as[FiniteDuration]("black-list-residence-time")
    val breakIdleConnectionsTimeout  = config.as[FiniteDuration]("break-idle-connections-timeout")
    val maxInboundConnections        = config.as[Int]("max-inbound-connections")
    val maxOutboundConnections       = config.as[Int]("max-outbound-connections")
    val maxConnectionsFromSingleHost = config.as[Int]("max-single-host-connections")
    val connectionTimeout            = config.as[FiniteDuration]("connection-timeout")
    val maxUnverifiedPeers           = config.as[Int]("max-unverified-peers")
    val enablePeersExchange          = config.as[Boolean]("enable-peers-exchange")
    val enableBlacklisting           = config.as[Boolean]("enable-blacklisting")
    val peersBroadcastInterval       = config.as[FiniteDuration]("peers-broadcast-interval")
    val handshakeTimeout             = config.as[FiniteDuration]("handshake-timeout")
    val suspensionResidenceTime      = config.as[FiniteDuration]("suspension-residence-time")
    val receivedTxsCacheTimeout      = config.as[FiniteDuration]("received-txs-cache-timeout")
    val uPnPSettings                 = config.as[UPnPSettings]("upnp")
    val trafficLogger                = config.as[TrafficLogger.Settings]("traffic-logger")

    NetworkSettings(
      file,
      bindAddress,
      declaredAddress,
      nodeName,
      nonce,
      knownPeers,
      peersDataResidenceTime,
      blackListResidenceTime,
      breakIdleConnectionsTimeout,
      maxInboundConnections,
      maxOutboundConnections,
      maxConnectionsFromSingleHost,
      connectionTimeout,
      maxUnverifiedPeers,
      enablePeersExchange,
      enableBlacklisting,
      peersBroadcastInterval,
      handshakeTimeout,
      suspensionResidenceTime,
      receivedTxsCacheTimeout,
      uPnPSettings,
      trafficLogger
    )
  }

  private def randomNonce: Long = {
    val base = 1000

    (Random.nextInt(base) + base) * Random.nextInt(base) + Random.nextInt(base)
  }
}
