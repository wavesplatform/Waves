package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import scorex.transaction._
import scorex.utils.NTP

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History with FeatureProvider, stateFile: Option[File], pageSplitSize: Int): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply[T](settings: WavesSettings,
               beforeStateUpdate: (BlockchainUpdater, StateReader, StateWriter) => T = (_: BlockchainUpdater, _: StateReader, _: StateWriter) => (),
               time: scorex.utils.Time = NTP):
    Try[(NgHistory with DebugNgHistory with AutoCloseable, FeatureProvider, AutoCloseable, StateReader, BlockchainUpdater, BlockchainDebugInfo, T)] =
  {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainSettings.blockchainFile, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
      ss <- createStateStorage(historyWriter, settings.blockchainSettings.stateFile, settings.mvstorePageSplitSize)
      stateWriter = new StateWriterImpl(ss, settings.blockchainSettings.storeTransactionsInState, lock)
      bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, time, lock)
      stateReader = bcu.bestLiquidState
      callbackResult = beforeStateUpdate(bcu, stateReader, stateWriter)
    } yield {
      bcu.syncPersistedAndInMemory()
      val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
      (history, history, stateWriter, stateReader, bcu, bcu, callbackResult)
    }
  }
}
