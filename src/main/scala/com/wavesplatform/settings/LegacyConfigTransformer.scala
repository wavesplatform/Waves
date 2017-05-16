package com.wavesplatform.settings

import java.io.File
import java.time.Duration
import scala.collection.convert.ImplicitConversionsToJava._
import scala.collection.convert.ImplicitConversionsToScala._
import com.google.common.base.CaseFormat
import com.typesafe.config.ConfigValueFactory.{fromAnyRef => cv}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions, ConfigValue, ConfigValueFactory}
import scorex.transaction.TransactionParser.TransactionType

object LegacyConfigTransformer {
  private val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)

  private def transformFees(feeMap: Config): ConfigValue = {
    val m = feeMap.root().keySet().map { txId =>
      val key = converter.convert(TransactionType(txId.toInt).toString).split("_")(0).split("-").dropRight(1).mkString("-")
      key -> feeMap.getValue(txId)
    }.toMap

    ConfigValueFactory.fromMap(m)
  }

  private def millis(cfg: Config, path: String) = cv(Duration.ofMillis(cfg.getLong(path)))

  private def seconds(cfg: Config, path: String) = cv(Duration.ofSeconds(cfg.getLong(path)))

  private def days(cfg: Config, path: String) = cv(Duration.ofDays(cfg.getLong(path)))

  private def defaultDir = s"${System.getProperty("user.home")}"

  private val transformers: Seq[(String, (Config, String) => ConfigValue)] = Seq(
    "p2p.peersDataResidenceTimeDays" -> { (cfg, p) => cv(Duration.ofDays(cfg.getInt(p))) },
    "p2p.blacklistResidenceTimeMilliseconds" -> millis,
    "p2p.peersDataBroadcastDelay" -> millis,
    "p2p.upnpGatewayTimeout" -> seconds,
    "p2p.upnpDiscoverTimeout" -> seconds,
    "p2p.connectionTimeout" -> seconds,
    "p2p.outboundBufferSizeMb" -> { (cfg, p) => cv(s"${cfg.getInt(p)}M") },
    "allowedGenerationTimeFromLastBlockInterval" -> days,
    "logLevel" -> { (cfg, p) => cv(cfg.getString(p).toUpperCase) },
    "feeMap" -> { (cfg, p) => transformFees(cfg.getConfig(p)) },
    "testnet" -> { (cfg, p) => cv(if (cfg.getBoolean(p)) "TESTNET" else "MAINNET") },
    "scoreBroadcastDelay" -> millis,
    "utxRebroadcastInterval" -> seconds,
    "historySynchronizerTimeout" -> seconds,
    "blockGenerationDelay" -> millis,
    "walletDir" -> { (cfg, p) =>
      val v = cfg.getString(p)
      val r = if (v.isEmpty) s"$defaultDir/wallet/wallet.dat" else s"$v/wallet.s.dat"
      cv(r)
    },
    "dataDir" -> { (cfg, p) =>
      val v = cfg.getString(p)
      val r = if (v.isEmpty) s"$defaultDir" else v.substring(0, v.lastIndexOf('/'))
      cv(r)
    }
  )

  private val fieldMap = Map(
    "dataDir" -> Seq("directory"),
    "logLevel" -> Seq("logging-level"),
    "feeMap" -> Seq("fees"),
    "testnet" -> Seq("blockchain.type"),

    "blacklistThreshold" -> Seq("network.black-list-threshold"),
    "p2p.localOnly" -> Seq("network.local-only"),
    "p2p.bindAddress" -> Seq("network.bind-address"),
    "p2p.upnp" -> Seq("network.upnp.enable"),
    "p2p.upnpGatewayTimeout" -> Seq("network.upnp.gateway-timeout"),
    "p2p.upnpDiscoverTimeout" -> Seq("network.upnp.discover-timeout"),
    "p2p.knownPeers" -> Seq("network.known-peers"),
    "p2p.port" -> Seq("network.port"),
    "p2p.nodeName" -> Seq("network.node-name"),
    "p2p.maxConnections" -> Seq("network.max-inbound-connections", "network.max-outbound-connections"),
    "p2p.connectionTimeout" -> Seq("network.connection-timeout"),
    "p2p.peersDataBroadcastDelay" -> Seq("network.peers-broadcast-interval"),
    "p2p.peersDataResidenceTimeDays" -> Seq("network.peers-data-residence-time"),
    "p2p.myAddress" -> Seq("network.declared-address"),
    "p2p.blacklistResidenceTimeMilliseconds" -> Seq("network.black-list-residence-time"),
    "p2p.outboundBufferSizeMb" -> Seq("network.outbound-buffer-size"),
    "p2p.minEphemeralPortNumber" -> Seq("network.min-ephemeral-port-number"),
    "p2p.maxUnverifiedPeers" -> Seq("network.max-unverified-peers"),

    "walletPassword" -> Seq("wallet.password"),
    "walletSeed" -> Seq("wallet.seed"),
    "walletDir" -> Seq("wallet.file"),

    "rpcEnabled" -> Seq("rest-api.enable"),
    "rpcPort" -> Seq("rest-api.port"),
    "rpcAddress" -> Seq("rest-api.bind-address"),
    "apiKeyHash" -> Seq("rest-api.api-key-hash"),
    "cors" -> Seq("rest-api.cors"),

    "minerEnabled" -> Seq("miner.enable"),
    "quorum" -> Seq("miner.quorum"),
    "offlineGeneration" -> Seq("miner.offline"),
    "blockGenerationDelay" -> Seq("miner.generation-delay"),
    "allowedGenerationTimeFromLastBlockInterval" -> Seq("miner.interval-after-last-block-then-generation-is-allowed"),
    "tflikeScheduling" -> Seq("miner.tf-like-scheduling"),

    "scoreBroadcastDelay" -> Seq("synchronization.score-broadcast-interval"),
    "historySynchronizerTimeout" -> Seq("synchronization.synchronization-timeout"),
    "maxRollback" -> Seq("synchronization.max-rollback"),
    "maxChain" -> Seq("synchronization.max-chain-length"),
    "loadEntireChain" -> Seq("synchronization.load-entire-chain"),
    "pinToInitialPeer" -> Seq("synchronization.pin-to-initial-peer"),
    "retriesBeforeBlacklisted" -> Seq("synchronization.retries-before-blacklisting"),
    "operationRetries" -> Seq("synchronization.operation-retires"),

    "utxSize" -> Seq("utx.size"),
    "utxRebroadcastInterval" -> Seq("utx.broadcast-interval"),

    "checkpoints.publicKey" -> Seq("checkpoints.public-key"))

  val legacyDefaults = ConfigFactory.parseString(
    """feeMap = {}
      |testnet = false
      |logLevel = info
      |blacklistThreshold = 50
      |minerEnabled = true
      |quorum = 1
      |allowedGenerationTimeFromLastBlockInterval = 1
      |tflikeScheduling = true
      |maxChain = 11
      |loadEntireChain = true
      |pinToInitialPeer = false
      |retriesBeforeBlacklisted = 2
      |operationRetries = 50
      |scoreBroadcastDelay = 30000
      |utxSize = 10000
      |utxRebroadcastInterval = 30
      |historySynchronizerTimeout = 30
      |blockGenerationDelay = 1000
      |p2p {
      |  localOnly = false
      |  peersDataResidenceTimeDays = 1
      |  blacklistResidenceTimeMilliseconds = 600000
      |  connectionTimeout = 60
      |  outboundBufferSizeMb = 15
      |  minEphemeralPortNumber = 32768
      |  maxUnverifiedPeers = 1000
      |  peersDataBroadcastDelay = 30000
      |  upnpGatewayTimeout = 100
      |  upnpDiscoverTimeout = 100
      |}
      |""".stripMargin)

  def transform(legacyConfig: Config): Config = {
    val transformedLegacyConfig = transformers.filter {
      case (path, _) => legacyConfig.hasPath(path)
    }.foldLeft(legacyConfig) {
      case (cfg, (path, f)) => cfg.withValue(path, f(cfg, path))
    }

    val mappedValues = fieldMap.foldLeft(ConfigFactory.empty()) {
      case (cfg, (sourceKey, destKeys)) =>
        if (transformedLegacyConfig.hasPath(sourceKey))
          destKeys.foldLeft(cfg) { case (c, destKey) =>
            c.withValue(destKey, transformedLegacyConfig.getValue(sourceKey))
          }
        else cfg
    }

    ConfigFactory.empty().withValue("waves", mappedValues.root())
  }

  def main(args: Array[String]): Unit =
    for (p <- args.headOption) {
      val cfg = ConfigFactory.parseFile(new File(p))
      val options = ConfigRenderOptions.defaults().setJson(false).setOriginComments(false)
      println(transform(cfg).root().render(options))
    }
}
