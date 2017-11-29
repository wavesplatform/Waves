package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import scorex.transaction._

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History with FeatureProvider, stateFile: Option[File], pageSplitSize: Int): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(settings: WavesSettings): Try[(NgHistory with DebugNgHistory with AutoCloseable, FeatureProvider, AutoCloseable, StateReader, BlockchainUpdater, BlockchainDebugInfo)] = {
    val lock = new RWL(true)
    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainSettings.blockchainFile, new RWL(true), settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
      ss <- createStateStorage(historyWriter, settings.blockchainSettings.stateFile, settings.mvstorePageSplitSize)
      stateWriter = new StateWriterImpl(ss, settings.blockchainSettings.storeTransactionsInState, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, lock)
      val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
      (history, history, stateWriter, bcu.bestLiquidState, bcu, bcu)
    }
  }
}
