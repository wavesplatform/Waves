package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.{FeaturesSettings, WavesSettings}
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import scorex.transaction._

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History with FeatureProvider, stateFile: Option[File]): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(settings: WavesSettings, featuresSettings: FeaturesSettings): Try[(NgHistory with DebugNgHistory with AutoCloseable, FeatureProvider, AutoCloseable, StateReader, BlockchainUpdater, BlockchainDebugInfo)] = {
    val lock = new RWL(true)

    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainSettings.blockchainFile, lock, settings.blockchainSettings.functionalitySettings, featuresSettings)
      ss <- createStateStorage(historyWriter, settings.blockchainSettings.stateFile)
      stateWriter = new StateWriterImpl(ss, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings, settings.blockchainSettings.minimumInMemoryDiffSize, lock)
      val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
      (history, history,stateWriter, bcu.bestLiquidState, bcu, bcu)
    }
  }
}
