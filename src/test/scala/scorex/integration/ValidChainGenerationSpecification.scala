package scorex.integration

import org.scalatest.FunSuite
import scorex.app.LagonakiApplication
import scorex.block.Block

//todo: fix test, problems with app stopping
class ValidChainGenerationSpecification extends FunSuite {
  ignore("retroactive chain test") {
    val application = new LagonakiApplication("settings-test.json")
    implicit val consensusModule = application.consensusModule
    implicit val transactionModule = application.transactionModule

    application.run()
    application.wallet.generateNewAccounts(10)
    require(application.wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(5000)
    val bh = application.blockchainImpl.height()

    //chain validity check
    (2 to bh).foreach { h =>
      assert(application.blockchainImpl.blockAt(h).get.isValid)
    }

    val b1 = application.blockchainImpl.blockAt(1).get
    val b2 = application.blockchainImpl.blockAt(2).get

    //empty block size check
    /*
    if (Constants.ConsensusAlgo == QoraLikeConsensusModule) {
      assert(b2.bytes.size == 309)
    } else if (Constants.ConsensusAlgo == NxtLikeConsensusModule) {
      assert(b2.bytes.size == 213)
    } */

    //toBytes/parse roundtrip test
    val bb2 = Block.parse(b2.bytes).get
    assert(bb2.timestampField.value == b2.timestampField.value)
    assert(b1.timestampField.value != b2.timestampField.value)
    assert(b1 != b2)

    //serialization/deserialization  thru BlockMessage roundtrip test
    /*
    val bytes = BlockMessage(2, b2).bytes
    if (Constants.ConsensusAlgo == QoraLikeConsensusModule) {
      assert(bytes.length == 326)
    } else if (Constants.ConsensusAlgo == NxtLikeConsensusModule) {
      assert(bytes.length == 230)
    }

    val restored = Message.parse(ByteBuffer.wrap(bytes)).get.asInstanceOf[BlockMessage].block
    assert(restored.timestampField.value == b2.timestampField.value)
    assert(restored.isValid()) */

    application.stopAll()
  }
}