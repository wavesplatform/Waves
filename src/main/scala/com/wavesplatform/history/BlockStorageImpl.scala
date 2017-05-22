package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl, _}
import org.h2.mvstore.MVStore
import scorex.transaction.{CheckpointService, History}
import scorex.utils.ScorexLogging

object BlockStorageImpl extends ScorexLogging {

  def apply(settings: BlockchainSettings): (CheckpointService, History, StateReader, BlockchainUpdaterImpl) = {

    val lock = new ReentrantReadWriteLock(true)

    val checkpointService = {
      val checkpointStore: MVStore = createMVStore(settings.checkpointFile)
      new CheckpointServiceImpl(checkpointStore)
    }

    val historyWriter = {
      val blockchainStore = createMVStore(settings.blockchainFile)
      HistoryWriterImpl(blockchainStore, lock) match {
        case Right(v) => v
        case Left(err) =>
          log.error(err)
          blockchainStore.closeImmediately()
          delete(settings.stateFile)
          delete(settings.blockchainFile)
          val recreatedStore = createMVStore(settings.blockchainFile)
          HistoryWriterImpl(recreatedStore, lock).explicitGet()
      }
    }

    @scala.annotation.tailrec
    def withStateMVStore(stateMVStore: MVStore, retry: Boolean): (CheckpointService, History, StateReader, BlockchainUpdaterImpl) = (for {
      stateStorage <- StateStorage(stateMVStore)
      persistedStateWriter = new StateWriterImpl(stateStorage, lock)
      bcUpdater <- BlockchainUpdaterImpl(persistedStateWriter, settings.functionalitySettings,
        settings.minimumInMemoryDiffSize, historyWriter, lock)
    } yield bcUpdater) match {
      case Right(bcu) => (checkpointService, historyWriter, bcu.currentState, bcu)
      case Left(err) =>
        if (retry) {
          log.error(err)
          stateMVStore.closeImmediately()
          delete(settings.stateFile)
          withStateMVStore(createMVStore(settings.stateFile), retry = false)
        } else throw new Exception(err)
    }

    withStateMVStore(createMVStore(settings.stateFile), retry = true)

  }

  private def delete(fileName: String) = {
    if (new File(fileName).delete()) {
      log.info(s"recreating $fileName")
    } else {
      throw new Exception(s"Unable to delete $fileName")
    }
  }

  private def createMVStore(fileName: String): MVStore = {
    def stringToOption(s: String) = Option(s).filter(_.trim.nonEmpty)

    stringToOption(fileName) match {
      case Some(s) =>
        val file = new File(s)
        file.getParentFile.mkdirs().ensuring(file.getParentFile.exists())

        new MVStore.Builder().fileName(s).compress().open()
      case None =>
        new MVStore.Builder().open()
    }
  }
}
