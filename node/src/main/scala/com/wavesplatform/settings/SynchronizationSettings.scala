package com.wavesplatform.settings

import com.wavesplatform.network.InvalidBlockStorageImpl.InvalidBlockStorageSettings
import com.wavesplatform.settings.SynchronizationSettings._

import scala.concurrent.duration.FiniteDuration

case class SynchronizationSettings(
    maxRollback: Int,
    synchronizationTimeout: FiniteDuration,
    scoreTTL: FiniteDuration,
    maxBaseTarget: Option[Long],
    invalidBlocksStorage: InvalidBlockStorageSettings,
    microBlockSynchronizer: MicroblockSynchronizerSettings,
    historyReplier: HistoryReplierSettings,
    utxSynchronizer: UtxSynchronizerSettings
)

object SynchronizationSettings {
  case class MicroblockSynchronizerSettings(
      waitResponseTimeout: FiniteDuration,
      processedMicroBlocksCacheTimeout: FiniteDuration,
      invCacheTimeout: FiniteDuration
  )

  case class HistoryReplierSettings(maxMicroBlockCacheSize: Int, maxBlockCacheSize: Int)

  case class UtxSynchronizerSettings(networkTxCacheSize: Int, maxThreads: Option[Int], maxQueueSize: Int, allowTxRebroadcasting: Boolean) {
    def poolSize: Int = maxThreads.getOrElse(Math.max(Runtime.getRuntime.availableProcessors() / 4, 1).min(4))
  }
}
