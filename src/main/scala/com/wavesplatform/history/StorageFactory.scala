package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.settings.{BlockchainSettings, FeaturesSettings}
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import scorex.transaction._

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History, stateFile: Option[File]): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(settings: BlockchainSettings, featuresSettings: FeaturesSettings): Try[(NgHistory with DebugNgHistory with AutoCloseable, AutoCloseable, StateReader, BlockchainUpdater, BlockchainDebugInfo)] = {
    val lock = new RWL(true)

    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainFile, lock, settings.functionalitySettings, featuresSettings)
      ss <- createStateStorage(historyWriter, settings.stateFile)
      stateWriter = new StateWriterImpl(ss, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock)
      (bcu.historyReader, stateWriter, bcu.bestLiquidState, bcu, bcu)
    }
  }
}
