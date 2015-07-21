package scorex.integration

import java.nio.ByteBuffer
import scorex.Controller
import Controller._
import org.scalatest.FunSuite
import scorex.block.Block
import scorex.consensus.{ConsensusModuleNxt, ConsensusModuleQora}
import scorex.network.message.{Message, BlockMessage}
import scorex.settings.{Constants, Settings}

class ValidChainGenerationSpecification extends FunSuite {
  test("retroactive chain test") {
    Settings.filename = "settings-test.json"
    Controller.init()
    wallet.generateNewAccounts(10)
    require(wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(15000)
    val bh = Controller.blockchainStorage.height()

    //chain validity check
    (2 to bh).foreach { h =>
      assert(Controller.blockchainStorage.blockAt(h).get.isValid())
      assert(Controller.blockchainStorage.blockAt(h).get.signatureValid)
    }

    val b1 = Controller.blockchainStorage.blockAt(1).get
    val b2 = Controller.blockchainStorage.blockAt(2).get

    //empty block size check
    if (Constants.ConsensusAlgo == ConsensusModuleQora) {
      assert(b2.bytes.size == 309)
    } else if (Constants.ConsensusAlgo == ConsensusModuleNxt) {
      assert(b2.bytes.size == 213)
    }

    //toBytes/parse roundtrip test
    val bb2 = Block.parse(b2.bytes).get
    assert(bb2.timestamp == b2.timestamp)
    assert(bb2.generator == b2.generator)

    assert(b1.timestamp != b2.timestamp)
    assert(b1 != b2)

    //serialization/deserialization  thru BlockMessage roundtrip test
    val bytes = BlockMessage(2, b2).bytes
    if (Constants.ConsensusAlgo == ConsensusModuleQora) {
      assert(bytes.length == 326)
    } else if (Constants.ConsensusAlgo == ConsensusModuleNxt) {
      assert(bytes.length == 230)
    }

    val restored = Message.parse(ByteBuffer.wrap(bytes)).get.asInstanceOf[BlockMessage].block
    assert(restored.timestamp == b2.timestamp)
    assert(restored.isValid())

    Controller.stopAll()
  }
}