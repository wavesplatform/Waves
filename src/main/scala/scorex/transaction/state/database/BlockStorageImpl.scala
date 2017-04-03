package scorex.transaction.state.database

import java.io.File

import com.wavesplatform.settings.BlockchainSettings
import org.h2.mvstore.MVStore
import scorex.consensus.nxt.WavesConsensusModule
import scorex.transaction._
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState}

class BlockStorageImpl(settings: BlockchainSettings)(implicit consensusModule: WavesConsensusModule, transactionModule: TransactionModule)
  extends BlockStorage {

  require(consensusModule != null)

  private def stringToOption(s: String) = Option(s).filter(_.trim.nonEmpty)

  private val database: MVStore = createMVStore(settings.file)

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

  protected[this] override val db: MVStore = database

  override val history: History = new StoredBlockchain(db)(consensusModule, transactionModule)

  override val state: State = StoredState.fromDB(db, settings.functionalitySettings)

}
