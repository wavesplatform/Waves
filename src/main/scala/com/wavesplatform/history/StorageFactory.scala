package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.settings.BlockchainSettings
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

  def apply(settings: BlockchainSettings): Try[(NgHistory with AutoCloseable, AutoCloseable, StateReader, BlockchainUpdater, BlockchainDebugInfo)] = {
    val lock = new RWL(true)

    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainFile, lock)
      ngHistoryWriter = new NgHistoryWriterImpl(historyWriter)
      ss <- createStateStorage(ngHistoryWriter, settings.stateFile)
      stateWriter = new StateWriterImpl(ss, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, ngHistoryWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock)
      (ngHistoryWriter, stateWriter, bcu.bestLiquidState, bcu, bcu)
    }
  }
}
