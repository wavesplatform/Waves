package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import scorex.transaction.History

import scala.util.Try

object BlockStorageImpl {

  private def createStateWriter(history: History, stateFile: Option[File], lock: RWL): Try[StateWriterImpl] = {
    StateStorage(stateFile)
      .map(new StateWriterImpl(_, lock))
      .filter(_.height <= history.height())
      .recoverWith {
        case _ =>
          StateStorage(stateFile, dropExisting = true)
            .map(new StateWriterImpl(_, lock))
            .filter(_.height <= history.height())
      }
  }

  def apply(settings: BlockchainSettings): Try[(CheckpointServiceImpl, HistoryWriterImpl, StateWriterImpl, BlockchainUpdaterImpl)] = {
    val lock = new RWL(true)

    for {
      historyWriter <- HistoryWriterImpl(settings.blockchainFile, lock)
      stateWriter <- createStateWriter(historyWriter, settings.stateFile, lock)
    } yield (
      new CheckpointServiceImpl(settings.checkpointFile),
      historyWriter,
      stateWriter,
      BlockchainUpdaterImpl(stateWriter, historyWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock))
  }
}
