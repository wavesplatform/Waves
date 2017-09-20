package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.features.Functionalities
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import scorex.transaction.{BlockchainUpdater, History}

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History, stateFile: Option[File]): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(settings: BlockchainSettings): Try[(History with AutoCloseable, AutoCloseable, StateReader, BlockchainUpdater, Functionalities)] = {
    val lock = new RWL(true)

    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainFile, lock)
      fn = new Functionalities(settings.functionalitySettings, historyWriter)
      ss <- createStateStorage(historyWriter, settings.stateFile)
      stateWriter = new StateWriterImpl(ss, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, fn, settings.minimumInMemoryDiffSize, lock)
      (historyWriter, stateWriter, bcu.currentPersistedBlocksState, bcu, fn)
    }
  }
}
