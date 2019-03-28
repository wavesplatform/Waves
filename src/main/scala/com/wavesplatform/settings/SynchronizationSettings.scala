package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.network.InvalidBlockStorageImpl.InvalidBlockStorageSettings
import com.wavesplatform.settings.SynchronizationSettings._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration.FiniteDuration

case class SynchronizationSettings(maxRollback: Int,
                                   maxChainLength: Int,
                                   synchronizationTimeout: FiniteDuration,
                                   scoreTTL: FiniteDuration,
                                   maxBaseTargetOpt: Option[Long],
                                   invalidBlocksStorage: InvalidBlockStorageSettings,
                                   microBlockSynchronizer: MicroblockSynchronizerSettings,
                                   historyReplierSettings: HistoryReplierSettings,
                                   utxSynchronizerSettings: UtxSynchronizerSettings)

object SynchronizationSettings {
  case class MicroblockSynchronizerSettings(waitResponseTimeout: FiniteDuration,
                                            processedMicroBlocksCacheTimeout: FiniteDuration,
                                            invCacheTimeout: FiniteDuration)

  case class HistoryReplierSettings(maxMicroBlockCacheSize: Int, maxBlockCacheSize: Int)

  case class UtxSynchronizerSettings(networkTxCacheSize: Int, networkTxCacheTime: FiniteDuration, maxBufferSize: Int, maxBufferTime: FiniteDuration)

  implicit val synchronizationSettingsValueReader: ValueReader[SynchronizationSettings] =
    (cfg, path) => fromConfig(cfg.getConfig(path))

  private[this] def fromConfig(config: Config): SynchronizationSettings = {
    val maxRollback             = config.as[Int]("max-rollback")
    val maxChainLength          = config.as[Int]("max-chain-length")
    val synchronizationTimeout  = config.as[FiniteDuration]("synchronization-timeout")
    val scoreTTL                = config.as[FiniteDuration]("score-ttl")
    val maxBaseTargetOpt        = config.as[Option[Long]]("max-base-target")
    val invalidBlocksStorage    = config.as[InvalidBlockStorageSettings]("invalid-blocks-storage")
    val microBlockSynchronizer  = config.as[MicroblockSynchronizerSettings]("micro-block-synchronizer")
    val historyReplierSettings  = config.as[HistoryReplierSettings]("history-replier")
    val utxSynchronizerSettings = config.as[UtxSynchronizerSettings]("utx-synchronizer")

    SynchronizationSettings(
      maxRollback,
      maxChainLength,
      synchronizationTimeout,
      scoreTTL,
      maxBaseTargetOpt,
      invalidBlocksStorage,
      microBlockSynchronizer,
      historyReplierSettings,
      utxSynchronizerSettings
    )
  }
}
