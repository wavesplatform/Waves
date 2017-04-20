package scorex.transaction.state.database

import java.io.File

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, MVStoreStateStorage, StateWriterImpl}
import org.h2.mvstore.MVStore
import scorex.transaction._
import scorex.transaction.state.database.blockchain.{CheckpointServiceImpl, HistoryWriterImpl, MVStoreCheckpointStorage, MVStoreHistoryStorage}

class BlockStorageImpl(settings: BlockchainSettings) extends BlockStorage {

  import BlockStorageImpl._

  val database: MVStore = createMVStore(settings.file)
  val h = new HistoryWriterImpl(new MVStoreHistoryStorage(database))
  val s = new StateWriterImpl(new MVStoreStateStorage(database))
  val c = new CheckpointServiceImpl(new MVStoreCheckpointStorage(database))
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
