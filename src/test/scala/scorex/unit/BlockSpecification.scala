package scorex.unit

import org.scalatest.FunSuite
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, BlockStub}
import scorex.consensus.nxt.{NxtBlockGenerationData, NxtBlockGenerationDataParser}
import scorex.consensus.qora.{QoraBlockGenerationData, QoraBlockGenerationDataParser}
import scorex.consensus.{ConsensusModuleNxt, ConsensusModuleQora}
import scorex.transaction.{PaymentTransaction, Transaction}
import settings.Constants

import scala.util.Random

class BlockSpecification extends FunSuite {
  test("block with txs toBytes/parse roundtrip") {
    val reference = Array.fill(Block.REFERENCE_LENGTH)(Random.nextInt(100).toByte)
    val gen = new PrivateKeyAccount(reference)
    val gd = (Constants.ConsensusAlgo match {
      case ConsensusModuleNxt =>
        val gs = Array.fill(NxtBlockGenerationDataParser.GENERATOR_SIGNATURE_LENGTH)(Random.nextInt(100).toByte)
        NxtBlockGenerationData(Random.nextLong(), gs)
      case ConsensusModuleQora =>
        val gs = Array.fill(QoraBlockGenerationDataParser.GENERATOR_SIGNATURE_LENGTH)(Random.nextInt(100).toByte)
        QoraBlockGenerationData(Random.nextLong(), gs)
    }).asInstanceOf[Constants.ConsensusAlgo.kernelData]
    val bs = BlockStub(1, reference, System.currentTimeMillis(), gen, gd)

    val sender = new PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction(sender, gen, BigDecimal(5), BigDecimal(1000), System.currentTimeMillis() - 5000)

    val b = Block(bs, Seq(tx), gen)

    val b2 = Block.parse(b.toBytes).get

    val at1 = b.transactions.head
    val at2 = b2.transactions.head

    assert(at1 == at2)
    assert(at1.amount == at2.amount)

  }
}