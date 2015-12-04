package scorex.lagonaki.integration

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import scorex.lagonaki.TestingCommons
import scorex.lagonaki.server.LagonakiApplication
import scorex.block.Block

class ValidChainGenerationSpecification extends FunSuite with TestingCommons {
  ignore("retroactive chain test") {
    val application = new LagonakiApplication(SettingsFilename)
    implicit val consensusModule = application.consensusModule
    implicit val transactionModule = application.transactionModule

    application.run()
    application.wallet.generateNewAccounts(10)
    require(application.wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(5000)
    val bh = application.history.height()

    //chain validity check
    (2 to bh).foreach { h =>
      assert(application.history.blockAt(h).get.isValid)
    }

    val b1 = application.history.blockAt(1).get
    val b2 = application.history.blockAt(2).get

    //toBytes/parse roundtrip test
    val bb2 = Block.parse(b2.bytes).get
    assert(bb2.timestampField.value == b2.timestampField.value)
    assert(b1.timestampField.value != b2.timestampField.value)
    assert(b1 != b2)

    //serialization/deserialization  thru BlockMessage roundtrip test

    application.messagesHandler.parse(ByteBuffer.wrap(b2.bytes), None).get.data.get match {
      case restored:Block =>
          assert (restored.timestampField.value == b2.timestampField.value)
          assert (restored.isValid)
      case _ => fail("wrong data type")
    }

    application.stopAll()
  }
}