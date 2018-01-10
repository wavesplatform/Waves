package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import scorex.transaction._
import scorex.utils.{NTP, Time}

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History with FeatureProvider, stateFile: Option[File], pageSplitSize: Int, time: Time): Try[StateStorage] =
    StateStorage(stateFile, time, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, time, dropExisting = true)
      }
    }

  def apply[T](settings: WavesSettings,
               beforeStateUpdate: (BlockchainDebugInfo) => T = (_: BlockchainDebugInfo) => (),
               time: Time = NTP):
    Try[(NgHistory with DebugNgHistory with AutoCloseable, FeatureProvider, AutoCloseable, StateReader, BlockchainUpdater, BlockchainDebugInfo, T)] =
  {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainSettings.blockchainFile, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
      ss <- createStateStorage(historyWriter, settings.blockchainSettings.stateFile, settings.mvstorePageSplitSize, time)
      stateWriter = new StateWriterImpl(ss, settings.blockchainSettings.storeTransactionsInState, lock)
      bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, time, lock)
      stateReader = bcu.bestLiquidState
      callbackResult = beforeStateUpdate(bcu)
    } yield {
      bcu.syncPersistedAndInMemory()
      val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
      (history, history, stateWriter, stateReader, bcu, bcu, callbackResult)
    }
  }
}
