package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import scorex.transaction.{BlockchainUpdater, History}

import scala.util.{Success, Try}

object BlockStorageImpl {

  private def createStateStorage(history: History, stateFile: Option[File]): Try[StateStorage] =
    StateStorage(stateFile).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(settings: BlockchainSettings): Try[(History with AutoCloseable, AutoCloseable, StateReader, BlockchainUpdater)] = {
    val lock = new RWL(true)

    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainFile, lock)
      ss <- createStateStorage(historyWriter, settings.stateFile)
      stateWriter = new StateWriterImpl(ss, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock)
      (historyWriter, stateWriter, bcu.currentState, bcu)
    }
  }
}
