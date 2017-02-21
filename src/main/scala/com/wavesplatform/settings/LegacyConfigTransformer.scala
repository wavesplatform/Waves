package com.wavesplatform.settings

import com.typesafe.config.{Config, ConfigFactory, ConfigValue, ConfigValueFactory}
import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.convert.ImplicitConversionsToJava._
import com.google.common.base.CaseFormat
import scorex.transaction.TypedTransaction.TransactionType

object LegacyConfigTransformer {
  private val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_UNDERSCORE)
  private def transformFees(feeMap: Config): ConfigValue = {
    val m = feeMap.root().keySet().map { txId =>
      val key = converter.convert(TransactionType(txId.toInt).toString).split("_")(0)
      key -> feeMap.getValue(txId)
    }.toMap

    ConfigValueFactory.fromMap(m)
  }

  private val fieldMap = Map(
    "dataDir" -> "waves.directory",
    "loggingLevel" -> "waves.logging-level",

    "p2p.localOnly"               -> "network.local-only",
    "p2p.bindAddress"             -> "network.bind-address",
    "p2p.upnp"                    -> "network.upnp.enable",
    "p2p.knownPeers"              -> "network.knownPeers",
    "p2p.port"                    -> "network.port",
    "p2p.nodeName"                -> "network.node-name",
    "p2p.maxConnections"          -> "network.max-connections",
    "p2p.peersDataBroadcastDelay" -> "network.peers-broadcast-interval",

    "walletPassword" -> "wallet.password",
    "walletSeed"     -> "wallet.seed",
    "walletDir"      -> "wallet.file",

    "rpcEnabled" -> "rest-api.enable",
    "rpcPort"    -> "rest-api.port",
    "rpcAddress" -> "rest-api.bind-address",
    "apiKeyHash" -> "rest-api.api-key-hash",
    "cors"       -> "rest-api.cors",

    "offlineGeneration"    -> "miner.offline",
    "blockGenerationDelay" -> "miner.generation-delay",

    "matcher.account"     -> "matcher.account",
    "matcher.host"        -> "matcher.bind-address",
    "matcher.port"        -> "matcher.port",
    "matcher.minOrderFee" -> "matcher.order-match-tx-fee",

    "scoreBroadcastDelay"        -> "synchronization.score-broadcast-interval",
    "historySynchronizerTimeout" -> "synchronization.synchronization-timeout",
    "maxRollback"                -> "synchronization.max-rollback",

    "checkpoints.publicKey" -> "checkpoints.public-key")

  def transform(legacyConfig: Config): Config = {
    val lc = legacyConfig.withFallback(ConfigFactory.parseString("feeMap = {}, testnet = false".stripMargin))

    val blockchainType = ConfigValueFactory.fromAnyRef(if (lc.getBoolean("testnet")) "TESTNET" else "MAINNET")

    fieldMap.foldLeft(ConfigFactory.empty()) {
      case (cfg, (sourceKey, destKey)) =>
        if (lc.hasPath(sourceKey))
          cfg.withValue(destKey, lc.getValue(sourceKey))
        else cfg
    }
      .withValue("blockchain.type", blockchainType)
      .withValue("fees", transformFees(lc.getConfig("feeMap")))
  }
}
