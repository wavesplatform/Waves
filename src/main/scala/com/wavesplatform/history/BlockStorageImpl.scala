package com.wavesplatform.history

import java.io.File

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import org.h2.mvstore.MVStore
import scorex.transaction._

class BlockStorageImpl(settings: BlockchainSettings) extends BlockStorage {

  import BlockStorageImpl._

  val blockchainStore: MVStore = createMVStore(settings.blockchainFile)
  val stateStore: MVStore = createMVStore(settings.stateFile)
  val checkpointStore: MVStore = createMVStore(settings.checkpointFile)
  val historyWriter = new HistoryWriterImpl(blockchainStore)
  val stateWriter = new StateWriterImpl(new StateStorage(stateStore))
  val checkpointService = new CheckpointServiceImpl(checkpointStore)
  val bcUpdater = new BlockchainUpdaterImpl(stateWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, historyWriter)


  override def history: History = historyWriter

  override def stateReader: StateReader = bcUpdater.currentState

  override def blockchainUpdater: BlockchainUpdater = bcUpdater

  override def checkpoints: CheckpointService = checkpointService
}

object BlockStorageImpl {

  private def stringToOption(s: String) = Option(s).filter(_.trim.nonEmpty)

  def createMVStore(fileName: String): MVStore = {
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
