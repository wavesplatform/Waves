package scorex.lagonaki.unit

import org.scalatest.{FunSuite, Matchers}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.lagonaki.TestingCommons
import scorex.transaction.SimpleTransactionModule
import scorex.transaction.state.database.blockchain.StoredBlockchain


class BlockchainStorageSpecification extends FunSuite with Matchers with TestingCommons {

  implicit val consensusModule = new NxtLikeConsensusModule
  implicit val transactionModule = new SimpleTransactionModule()

  val blockchainStorage = new StoredBlockchain(None)


  test("genesis block save & find") {
    transactionModule.blockStorage.appendBlock(Block.genesis())
    blockchainStorage.height() should be(1)
  }
}
