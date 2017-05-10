package com.wavesplatform.history

import java.io.File

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import org.h2.mvstore.MVStore
import scorex.transaction.{CheckpointService, History}
import scorex.utils.ScorexLogging
import com.wavesplatform.state2._

object BlockStorageImpl extends ScorexLogging {

  def apply(settings: BlockchainSettings): (CheckpointService, History, StateReader, BlockchainUpdaterImpl) = {

    val checkpointService = {
      val checkpointStore: MVStore = createMVStore(settings.checkpointFile)
      new CheckpointServiceImpl(checkpointStore)
    }

    val historyWriter = {
      val blockchainStore = createMVStore(settings.blockchainFile)
      HistoryWriterImpl(blockchainStore).left.flatMap { err =>
        log.error(err + s", recreating ${settings.stateFile} and ${settings.blockchainFile}")
        blockchainStore.closeImmediately()
        delete(settings.stateFile)
        delete(settings.blockchainFile)
        HistoryWriterImpl(createMVStore(settings.blockchainFile))
      }.explicitGet()
    }

    val stateWriter = new StateWriterImpl({
      val stateStore = createMVStore(settings.stateFile)
      StateStorage(stateStore).left.flatMap { err =>
        log.error(err + s", recreating ${settings.stateFile}")
        stateStore.closeImmediately()
        delete(settings.stateFile)
        StateStorage(createMVStore(settings.stateFile))
      }.explicitGet()
    })

    val bcUpdater = new BlockchainUpdaterImpl(stateWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, historyWriter)

    (checkpointService, historyWriter, bcUpdater.currentState, bcUpdater)
  }

  private def delete(fileName: String) = {
    if (!new File(fileName).delete()) {
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
