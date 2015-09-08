package scorex.unit

import org.scalatest.{Matchers, FunSuite}
import play.api.libs.json.{Json, JsObject}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.transaction.state.database.blockchain.StoredBlockchain
import scorex.transaction.{SimpleTransactionModule, TransactionSettings}


class BlockchainStorageSpecification extends FunSuite with Matchers {

  implicit object TestTransactionLayerSettings extends TransactionSettings{
    override val settingsJSON: JsObject = Json.obj()
  }

  implicit val consensusModule = new NxtLikeConsensusModule
  implicit val transactionModule = new SimpleTransactionModule()

  val blockchainStorage = new StoredBlockchain(None)


  test("genesis block save & find") {
    blockchainStorage.appendBlock(Block.genesis())
    blockchainStorage.height() should be (1)
  }
}
