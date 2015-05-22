package scorex.integration

import controller.Controller
import controller.Controller._
import org.scalatest.FunSuite
import scorex.database.blockchain.PrunableBlockchainStorage
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
    Controller.stopAll()

  }
}