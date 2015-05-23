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

    Thread.sleep(10000)
    val bh = PrunableBlockchainStorage.height()
    (2 to bh).foreach{h =>
      assert(PrunableBlockchainStorage.blockAt(h).get.isValid())
      assert(PrunableBlockchainStorage.blockAt(h).get.isSignatureValid())
    }

    val b2 = PrunableBlockchainStorage.blockAt(2).get

    val bytes = BlockMessage(2, b2).toBytes()
    assert(BlockMessage(bytes).block.isValid())

    Controller.stopAll()
  }
}