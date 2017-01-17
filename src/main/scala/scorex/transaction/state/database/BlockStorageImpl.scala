package scorex.transaction.state.database

import org.h2.mvstore.MVStore
import scorex.consensus.ConsensusModule
import scorex.network.StoredBlockSeq
import scorex.settings.{Settings, WavesHardForkParameters}
import scorex.transaction._
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState}

class BlockStorageImpl(settings: TransactionSettings with Settings,
                       forksParams: WavesHardForkParameters)
                      (implicit consensusModule: ConsensusModule[_], transactionModule: TransactionModule[_])
  extends BlockStorage {

  require(consensusModule != null)

  private val database: MVStore = createMVStore(Option("blockchain.dat"))

  def createMVStore(fileName: Option[String]): MVStore = {
    settings.dataDirOpt.flatMap(dir => fileName.map(dir + '/' + _)) match {
      case Some(pathToDataFile) =>
        new MVStore.Builder().fileName(pathToDataFile).compress().open()
      case None =>
        new MVStore.Builder().open()
    }
  }

  protected[this] override val db = database

  override val history: History = settings.history match {
    case s: String if s.equalsIgnoreCase("blockchain") =>
      new StoredBlockchain(db)(consensusModule, transactionModule)
    case s =>
      log.error(s"Unknown history storage: $s. Use StoredBlockchain...")
      new StoredBlockchain(db)(consensusModule, transactionModule)
  }

  override val state = StoredState.fromDB(db, forksParams)

  override val blockSeq = new StoredBlockSeq(createMVStore(settings.chainFileName))
}
