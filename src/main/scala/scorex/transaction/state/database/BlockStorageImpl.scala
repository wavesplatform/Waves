package scorex.transaction.state.database

import java.io.File

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.{BlockchainUpdaterImpl, MVStorePrimitiveImpl, StateWriterImpl}
import com.wavesplatform.state2.reader.{StateReader, StateReaderImpl}
import org.h2.mvstore.MVStore
import scorex.transaction._
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState}

class BlockStorageImpl(settings: BlockchainSettings) extends BlockStorage {

  import BlockStorageImpl._

  val database: MVStore = createMVStore(settings.file)
  val h: History = new StoredBlockchain(database)
  val mVStorePrimitiveImpl = new MVStorePrimitiveImpl(database)
  val rw = new StateWriterImpl(mVStorePrimitiveImpl)
  val updater = new BlockchainUpdaterImpl(rw, settings.functionalitySettings, h)

  override val history: History = h

  override val state: State = ???

  override def stateReader: StateReader = updater.composite

  override def blockchainUpdater: BlockchainUpdater = updater
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
