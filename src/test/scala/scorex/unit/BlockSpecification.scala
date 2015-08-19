package scorex.unit

import org.scalatest.FunSuite
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, BlockStub}
import scorex.consensus.nxt.{NxtBlockGenerationData, NxtBlockGenerationDataParser}
import scorex.consensus.qora.{QoraBlockGenerationData, QoraBlockGenerationFunctions}
import scorex.consensus.{ConsensusModuleNxt, ConsensusModuleQora}
import scorex.crypto.Base58
import scorex.app.settings.Constants
import scorex.transaction.{PaymentTransaction, Transaction}

import scala.util.Random

class BlockSpecification extends FunSuite {
  test("block with txs toBytes/parse roundtrip") {
    val reference = Array.fill(Block.ReferenceLength)(Random.nextInt(100).toByte)
    val gen = new PrivateKeyAccount(reference)
    val gd = (Constants.ConsensusAlgo match {
      case ConsensusModuleNxt =>
        val gs = Array.fill(NxtBlockGenerationDataParser.GeneratorSignatureLength)(Random.nextInt(100).toByte)
        NxtBlockGenerationData(Random.nextInt(Int.MaxValue) + 1, gs)

      case ConsensusModuleQora =>
        val gb = Random.nextInt(Int.MaxValue) + 1
        val gs = QoraBlockGenerationFunctions.calculateSignature(reference, gb, gen)
        QoraBlockGenerationData(gb, gs)
    }).asInstanceOf[Constants.ConsensusAlgo.kernelData]

    val bs = BlockStub(1, reference, System.currentTimeMillis(), gen, gd)

    val sender = new PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction(sender, gen, 5, 1000, System.currentTimeMillis() - 5000)

    val b = Block(bs, Seq(tx), gen)

    val b2 = Block.parse(b.bytes).get

    val at1 = b.transactions.head
    val at2 = b2.transactions.head

    assert(at1 == at2)
    assert(at1.amount == at2.amount)
    assert(b.signatureValid)
    assert(b2.signatureValid)
  }

  test("restoring from Base58 form") {
    if (Constants.ConsensusAlgo == ConsensusModuleNxt) {
      val b58 = "9FCfN5CGJPFtD8yt7Z26hbMfT3g2W52M2medVw8onEPbZHBa76TKnX7GaBNRbdcBVjv1cJ1ERQzhcNbf4BvU4jBNQ65x9bq4Btt5MQN7eXiEEGn6EXNsEXNURJz5UrNXefWw2bi2jwQV4fPF54eFw9EQYCnU2yzMZqWNjZJQNoouFszbzijEyjPreU1gSjJDeRvHHzP5rPnbqCr7QpoHCj4zNQ1LzbDfc5ahUer3Kn9xDgDbogMDZFgPw5E1KfNNB7N7vA3emqdyysab6EVDV3NKiy1MWL16d3"
      val b = Block.parse(Base58.decode(b58).get).get
      assert(b.signatureValid)
    }
  }
}