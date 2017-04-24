package com.wavesplatform.history

import java.io.File

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, MVStoreStateStorage, StateWriterImpl}
import org.h2.mvstore.MVStore
import scorex.transaction._

class BlockStorageImpl(settings: BlockchainSettings) extends BlockStorage {

  import BlockStorageImpl._

  val hdatabase: MVStore = createMVStore(settings.blockchainFile)
  val sdatabase: MVStore = createMVStore(settings.stateFile)
  val cdatabase: MVStore = createMVStore(settings.checkpointFile)
  val h = new HistoryWriterImpl(new MVStoreHistoryStorage(hdatabase))
  val s = new StateWriterImpl(new MVStoreStateStorage(sdatabase))
  val c = new CheckpointServiceImpl(new MVStoreCheckpointStorage(cdatabase))
  val updater = new BlockchainUpdaterImpl(s, settings.functionalitySettings, h)


  override def history: History = h

  override def stateReader: StateReader = updater.currentState

  override def blockchainUpdater: BlockchainUpdater = updater

  override def checkpoints: CheckpointService = c
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
