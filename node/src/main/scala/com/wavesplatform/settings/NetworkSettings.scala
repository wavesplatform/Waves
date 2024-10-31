package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.network.TrafficLogger
import com.wavesplatform.utils.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import net.ceedubs.ficus.readers.ValueReader
import pureconfig.*
import pureconfig.generic.auto.*

import java.io.File
import java.net.{InetSocketAddress, URI}
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

case class UPnPSettings(enable: Boolean, gatewayTimeout: FiniteDuration, discoverTimeout: FiniteDuration)

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
  private val MaxNodeNameBytesLength = 127

  implicit val configReader: ConfigReader[(NetworkSettings)] = ConfigReader.fromCursor[NetworkSettings] { cur =>
    for {
      objCur <- cur.asObjectCursor

      file        <- objCur.atKey("file").flatMap(ConfigReader[Option[File]].from)
      bindAddress <- objCur.atKey("bind-address").flatMap(ConfigReader[Option[String]].from)
      port        <- objCur.atKey("port").flatMap(ConfigReader[Option[Int]].from)
      bindAddress1 = for {
        addr <- bindAddress
        p    <- port
      } yield new InetSocketAddress(addr, p)

      declaredAddress <- objCur
        .atKey("declared-address")
        .flatMap(ConfigReader[Option[String]].from)
        .map(_.map { address =>
          val uri = new URI(s"my://$address")
          new InetSocketAddress(uri.getHost, uri.getPort)
        })

      nonce                       <- objCur.atKey("nonce").flatMap(ConfigReader[Option[Long]].from).map(_.getOrElse(randomNonce))
      nodeName                    <- objCur.atKey("node-name").flatMap(ConfigReader[Option[String]].from).map(_.getOrElse(s"Node-$nonce"))
      knownPeers                  <- objCur.atKey("known-peers").flatMap(ConfigReader[Seq[String]].from)
      peersDataResidenceTime      <- objCur.atKey("peers-data-residence-time").flatMap(ConfigReader[FiniteDuration].from)
      blackListResidenceTime      <- objCur.atKey("black-list-residence-time").flatMap(ConfigReader[FiniteDuration].from)
      breakIdleConnectionsTimeout <- objCur.atKey("break-idle-connections-timeout").flatMap(ConfigReader[FiniteDuration].from)
      maxInboundConnections       <- objCur.atKey("max-inbound-connections").flatMap(ConfigReader[Int].from)
      maxOutboundConnections      <- objCur.atKey("max-outbound-connections").flatMap(ConfigReader[Int].from)
      maxConnectionsPerHost       <- objCur.atKey("max-single-host-connections").flatMap(ConfigReader[Int].from)
      minConnections              <- objCur.atKey("min-connections").flatMap(ConfigReader[Option[Int]].from)
      connectionTimeout           <- objCur.atKey("connection-timeout").flatMap(ConfigReader[FiniteDuration].from)
      maxUnverifiedPeers          <- objCur.atKey("max-unverified-peers").flatMap(ConfigReader[Int].from)
      enablePeersExchange         <- objCur.atKey("enable-peers-exchange").flatMap(ConfigReader[Boolean].from)
      enableBlacklisting          <- objCur.atKey("enable-blacklisting").flatMap(ConfigReader[Boolean].from)
      peersBroadcastInterval      <- objCur.atKey("peers-broadcast-interval").flatMap(ConfigReader[FiniteDuration].from)
      handshakeTimeout            <- objCur.atKey("handshake-timeout").flatMap(ConfigReader[FiniteDuration].from)
      suspensionResidenceTime     <- objCur.atKey("suspension-residence-time").flatMap(ConfigReader[FiniteDuration].from)
      receivedTxsCacheTimeout     <- objCur.atKey("received-txs-cache-timeout").flatMap(ConfigReader[FiniteDuration].from)
      uPnPSettings                <- objCur.atKey("upnp").flatMap(ConfigReader[UPnPSettings].from)
      trafficLogger               <- objCur.atKey("traffic-logger").flatMap(ConfigReader[TrafficLogger.Settings].from)
    } yield NetworkSettings(
      file = file,
      bindAddress = bindAddress1,
      declaredAddress = declaredAddress,
      nodeName = nodeName,
      nonce = nonce,
      knownPeers = knownPeers,
      peersDataResidenceTime = peersDataResidenceTime,
      blackListResidenceTime = blackListResidenceTime,
      breakIdleConnectionsTimeout = breakIdleConnectionsTimeout,
      maxInboundConnections = maxInboundConnections,
      maxOutboundConnections = maxOutboundConnections,
      maxConnectionsPerHost = maxConnectionsPerHost,
      minConnections = minConnections,
      connectionTimeout = connectionTimeout,
      maxUnverifiedPeers = maxUnverifiedPeers,
      enablePeersExchange = enablePeersExchange,
      enableBlacklisting = enableBlacklisting,
      peersBroadcastInterval = peersBroadcastInterval,
      handshakeTimeout = handshakeTimeout,
      suspensionResidenceTime = suspensionResidenceTime,
      receivedTxsCacheTimeout = receivedTxsCacheTimeout,
      uPnPSettings = uPnPSettings,
      trafficLogger = trafficLogger
    )
  }

  implicit val valueReader: ValueReader[NetworkSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private[this] def fromConfig(config: Config): NetworkSettings = {
    val file        = config.getAs[File]("file")
    val bindAddress = config.getAs[String]("bind-address").map(addr => new InetSocketAddress(addr, config.as[Int]("port")))
    val nonce       = config.getOrElse("nonce", randomNonce)
    val nodeName    = config.getOrElse("node-name", s"Node-$nonce")
    require(nodeName.utf8Bytes.length <= MaxNodeNameBytesLength, s"Node name should have length less than $MaxNodeNameBytesLength bytes")
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
    val minConnections               = config.getAs[Int]("min-connections")
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
      minConnections,
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
