package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.network.InvalidBlockStorageImpl.InvalidBlockStorageSettings
import com.wavesplatform.settings.SynchronizationSettings._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

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

  case class UtxSynchronizerSettings(networkTxCacheSize: Int,
                                     networkTxCacheTime: FiniteDuration,
                                     maxBufferSize: Int,
                                     maxBufferTime: FiniteDuration,
                                     parallelism: Int,
                                     maxThreads: Int,
                                     maxQueueSize: Int)

  val configPath: String = "waves.synchronization"

  def fromConfig(config: Config): SynchronizationSettings = {
    val maxRollback             = config.as[Int](s"$configPath.max-rollback")
    val maxChainLength          = config.as[Int](s"$configPath.max-chain-length")
    val synchronizationTimeout  = config.as[FiniteDuration](s"$configPath.synchronization-timeout")
    val scoreTTL                = config.as[FiniteDuration](s"$configPath.score-ttl")
    val maxBaseTargetOpt        = config.as[Option[Long]](s"$configPath.max-base-target")
    val invalidBlocksStorage    = config.as[InvalidBlockStorageSettings](s"$configPath.invalid-blocks-storage")
    val microBlockSynchronizer  = config.as[MicroblockSynchronizerSettings](s"$configPath.micro-block-synchronizer")
    val historyReplierSettings  = config.as[HistoryReplierSettings](s"$configPath.history-replier")
    val utxSynchronizerSettings = config.as[UtxSynchronizerSettings](s"$configPath.utx-synchronizer")

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
