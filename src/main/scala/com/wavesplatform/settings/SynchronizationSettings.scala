package com.wavesplatform.settings

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
                                   historyReplier: HistoryReplierSettings,
                                   utxSynchronizer: UtxSynchronizerSettings)

object SynchronizationSettings {
  case class MicroblockSynchronizerSettings(waitResponseTimeout: FiniteDuration,
                                            processedMicroBlocksCacheTimeout: FiniteDuration,
                                            invCacheTimeout: FiniteDuration)

  case class HistoryReplierSettings(maxMicroBlockCacheSize: Int, maxBlockCacheSize: Int)

  case class UtxSynchronizerSettings(networkTxCacheSize: Int,
                                     maxBufferSize: Int,
                                     maxBufferTime: FiniteDuration,
                                     parallelism: Int,
                                     maxThreads: Int,
                                     maxQueueSize: Int)

  implicit val synchronizationSettingsValueReader: ValueReader[SynchronizationSettings] = arbitraryTypeValueReader
}
