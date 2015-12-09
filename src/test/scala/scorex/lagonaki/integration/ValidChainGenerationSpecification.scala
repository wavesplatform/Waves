package scorex.lagonaki.integration

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import scorex.block.Block
import scorex.lagonaki.TestingCommons
import scorex.transaction.BlockChain

class ValidChainGenerationSpecification extends FunSuite with TestingCommons {
  ignore("retroactive chain test") {
    implicit val consensusModule = application.consensusModule
    implicit val transactionModule = application.transactionModule

    application.run()
    application.wallet.generateNewAccounts(10)
    require(application.wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(5000)
    val bh = application.blockStorage.history.height()

    application.blockStorage.history match {
      case blochchain: BlockChain =>
        //chain validity check
        (2 to bh).foreach { h =>
          assert(blochchain.blockAt(h).get.isValid)
        }

        val b1 = blochchain.blockAt(1).get
        val b2 = blochchain.blockAt(2).get

        //toBytes/parse roundtrip test
        val bb2 = Block.parse(b2.bytes).get
        assert(bb2.timestampField.value == b2.timestampField.value)
        assert(b1.timestampField.value != b2.timestampField.value)
        assert(b1 != b2)

        //serialization/deserialization  thru BlockMessage roundtrip test

        application.messagesHandler.parse(ByteBuffer.wrap(b2.bytes), None).get.data.get match {
          case restored: Block =>
            assert(restored.timestampField.value == b2.timestampField.value)
            assert(restored.isValid)
          case _ => fail("wrong data type")
        }

        application.stopAll()
    }
  }
}