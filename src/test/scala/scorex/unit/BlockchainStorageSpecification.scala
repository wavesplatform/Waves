package scorex.unit

import org.scalatest.{Matchers, FunSuite}
import play.api.libs.json.{Json, JsObject}
import scorex.TestingCommons
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.transaction.state.database.blockchain.StoredBlockchain
import scorex.transaction.{SimpleTransactionModule, TransactionSettings}


class BlockchainStorageSpecification extends FunSuite with Matchers with TestingCommons {

  implicit val consensusModule = new NxtLikeConsensusModule
  implicit val transactionModule = new SimpleTransactionModule()

  val blockchainStorage = new StoredBlockchain(None)


  test("genesis block save & find") {
    blockchainStorage.appendBlock(Block.genesis())
    blockchainStorage.height() should be (1)
  }
}
