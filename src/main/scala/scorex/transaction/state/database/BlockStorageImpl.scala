package scorex.transaction.state.database

import java.io.File

import com.wavesplatform.settings.BlockchainSettings
import org.h2.mvstore.MVStore
import scorex.transaction._
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState}

class BlockStorageImpl(settings: BlockchainSettings) extends BlockStorage {

  import BlockStorageImpl._

  private val database: MVStore = createMVStore(settings.file)

  protected[this] override val db: MVStore = database

  override val history: History = new StoredBlockchain(db)

  override val state: State = StoredState.fromDB(db, settings.functionalitySettings)

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
