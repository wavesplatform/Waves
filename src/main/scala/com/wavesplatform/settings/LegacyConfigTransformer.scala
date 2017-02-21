package com.wavesplatform.settings

import java.time.Duration
import scala.collection.convert.ImplicitConversionsToJava._
import scala.collection.convert.ImplicitConversionsToScala._
import com.google.common.base.CaseFormat
import com.typesafe.config.ConfigValueFactory.{fromAnyRef => cv}
import com.typesafe.config.{Config, ConfigFactory, ConfigValue, ConfigValueFactory}
import scorex.transaction.TypedTransaction.TransactionType

object LegacyConfigTransformer {
  private val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)
  private def transformFees(feeMap: Config): ConfigValue = {
    val m = feeMap.root().keySet().map { txId =>
      val key = converter.convert(TransactionType(txId.toInt).toString).split("_")(0)
      key -> feeMap.getValue(txId)
    }.toMap

    ConfigValueFactory.fromMap(m)
  }

  private def millis(cfg: Config, path: String) = cv(Duration.ofMillis(cfg.getLong(path)))
  private def seconds(cfg: Config, path: String) = cv(Duration.ofSeconds(cfg.getLong(path)))
  private def days(cfg: Config, path: String) = cv(Duration.ofDays(cfg.getLong(path)))

  private val transformers: Seq[(String, (Config, String) => ConfigValue)] = Seq(
    "p2p.peersDataResidenceTimeDays" -> { (cfg, p) => cv(Duration.ofDays(cfg.getInt(p))) },
    "p2p.blacklistResidenceTimeMilliseconds" -> millis,
    "p2p.peersDataBroadcastDelay" -> millis,
    "p2p.upnpGatewayTimeout"  -> seconds,
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
    "blockGenerationDelay" -> millis
  )

  private val fieldMap = Map(
    "dataDir" -> "directory",
    "logLevel" -> "logging-level",
    "feeMap" -> "fees",
    "testnet" -> "blockchain.type",

    "blacklistThreshold"             -> "network.black-list-threshold",
    "p2p.localOnly"                  -> "network.local-only",
    "p2p.bindAddress"                -> "network.bind-address",
    "p2p.upnp"                       -> "network.upnp.enable",
    "p2p.upnpGatewayTimeout"         -> "network.upnp.gateway-timeout",
    "p2p.upnpDiscoverTimeout"        -> "network.upnp.discover-timeout",
    "p2p.knownPeers"                 -> "network.known-peers",
    "p2p.port"                       -> "network.port",
    "p2p.nodeName"                   -> "network.node-name",
    "p2p.maxConnections"             -> "network.max-connections",
    "p2p.connectionTimeout"          -> "network.connection-timeout",
    "p2p.peersDataBroadcastDelay"    -> "network.peers-broadcast-interval",
    "p2p.peersDataResidenceTimeDays" -> "network.peers-data-residence-time",
    "p2p.myAddress"                  -> "network.declared-address",
    "p2p.blacklistResidenceTimeMilliseconds" -> "network.black-list-residence-time",
    "p2p.outboundBufferSizeMb"       -> "network.outbound-buffer-size",
    "p2p.minEphemeralPortNumber"     -> "network.min-ephemeral-port-number",
    "p2p.maxUnverifiedPeers"         -> "network.max-unverified-peers",

    "walletPassword" -> "wallet.password",
    "walletSeed"     -> "wallet.seed",
    "walletDir"      -> "wallet.file",

    "rpcEnabled" -> "rest-api.enable",
    "rpcPort"    -> "rest-api.port",
    "rpcAddress" -> "rest-api.bind-address",
    "apiKeyHash" -> "rest-api.api-key-hash",
    "cors"       -> "rest-api.cors",

    "minerEnabled"         -> "miner.enable",
    "quorum"               -> "miner.quorum",
    "offlineGeneration"    -> "miner.offline",
    "blockGenerationDelay" -> "miner.generation-delay",
    "allowedGenerationTimeFromLastBlockInterval" -> "miner.interval-after-last-block-then-generation-is-allowed",
    "tflikeScheduling"     -> "miner.tf-like-scheduling",

    "matcher.account"     -> "matcher.account",
    "matcher.host"        -> "matcher.bind-address",
    "matcher.port"        -> "matcher.port",
    "matcher.minOrderFee" -> "matcher.order-match-tx-fee",

    "scoreBroadcastDelay"        -> "synchronization.score-broadcast-interval",
    "historySynchronizerTimeout" -> "synchronization.synchronization-timeout",
    "maxRollback"                -> "synchronization.max-rollback",
    "maxChain"                   -> "synchronization.max-chain-length",
    "loadEntireChain"            -> "synchronization.load-entire-chain",
    "pinToInitialPeer"           -> "synchronization.pin-to-initial-peer",
    "retriesBeforeBlacklisted"   -> "synchronization.retries-before-blacklisting",
    "operationRetries"           -> "synchronization.operation-retires",

    "utxSize"                -> "utx.size",
    "utxRebroadcastInterval" -> "utx.broadcast-interval",

    "checkpoints.publicKey" -> "checkpoints.public-key")

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
    val lc = transformers.foldLeft(legacyConfig.withFallback(legacyDefaults)) {
      case (cfg, (path, f)) => cfg.withValue(path, f(cfg, path))
    }

    val allValues = fieldMap.foldLeft(ConfigFactory.empty()) {
      case (cfg, (sourceKey, destKey)) =>
        if (lc.hasPath(sourceKey))
          cfg.withValue(destKey, lc.getValue(sourceKey))
        else cfg
    }

    ConfigFactory.empty().withValue("waves", allValues.root())
  }
}
