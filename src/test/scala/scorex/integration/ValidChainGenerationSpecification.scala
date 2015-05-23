package scorex.integration

import controller.Controller
import controller.Controller._
import org.scalatest.FunSuite
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.network.message.BlockMessage
import settings.Settings

//todo: clear environment after test
class ValidChainGenerationSpecification extends FunSuite {
  test("retroactive chain test") {
    Settings.filename = "settings-test.json"
    Controller.init()
    wallet.generateNewAccounts(10)
    require(wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(5000)
    val bh = PrunableBlockchainStorage.height()
    (2 to bh).foreach{h =>
      assert(PrunableBlockchainStorage.blockAt(h).get.isValid())
      assert(PrunableBlockchainStorage.blockAt(h).get.isSignatureValid())
    }

    val b1 = PrunableBlockchainStorage.blockAt(1).get
    val b2 = PrunableBlockchainStorage.blockAt(2).get

    assert(b1.timestamp != b2.timestamp)
    assert(b1 != b2)

    val bytes = BlockMessage(2, b2).toBytes()
    val restored = BlockMessage(bytes).block
    println(s"b2: $b2")
    println(s"restored: $restored")
    assert(restored.timestamp == b2.timestamp)
    //assert(restored.isValid())

    Controller.stopAll()
  }
}